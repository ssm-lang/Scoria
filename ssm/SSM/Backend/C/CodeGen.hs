{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SSM.Backend.C.CodeGen ( compile_ ) where

import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Data.Either                    ( rights )
import qualified Data.Map                      as Map

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           SSM.Core.Syntax
import SSM.Backend.C.Identifiers
import SSM.Backend.C.Exp

import           Debug.Trace

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | This function takes a `Program` and returns a pair where the first component is
-- the compiled program and the second is a list of all include statements.
compile_ :: Program -> ([C.Definition], [C.Definition])
compile_ program = (compUnit, includes)
 where
  compUnit = preamble ++ decls ++ defns

  preamble = genPreamble
  (decls, defns) =
    concat2 $ unzip $ map genProcedure $ Map.elems (funs program)
  concat2 (x, y) = (concat x, concat y)

-- | State maintained while compiling a 'Procedure'.
--
-- The information here is populated while generating the step function, so that
-- should be computed first, before this information is used to generate the act
-- struct and enter definitions.
data TRState = TRState
  { procedure :: Procedure        -- ^ Procedure we are compiling
  , ncase     :: Int              -- ^ Which number has the next case?
  , numwaits  :: Int              -- ^ The size of the widest wait
  , locals    :: [(Ident, Type)]  -- ^ Local references declared with var
  }

-- | Translation monad.
type TR a = State TRState a

-- | Run a TR computation on a procedure.
runTR :: Procedure -> TR a -> a
runTR p tra = evalState tra $ TRState p 0 0 []

-- | Read and increment the number of cases in a procedure, i.e., ncase++.
nextCase :: TR Int
nextCase = do
  n <- gets ncase
  modify $ \st -> st { ncase = n + 1 }
  return n

-- | Register a local variable for which an sv should be allocated.
addLocal :: Ident -> Type -> TR ()
addLocal n t = modify $ \st -> st { locals = (n, t) : locals st }

-- | Maintain the maximum number of variables a 'Procedure' waits on.
maxWaits :: Int -> TR ()
maxWaits rs = modify $ \st -> st { numwaits = rs `max` numwaits st }

{-------- Code generation --------}

-- | Generate include statements, to be placed at the top of the generated C.
genPreamble :: [C.Definition]
genPreamble = [cunit|

/**
 * Circumvent optimizations that take advantage of C's undefined signed
 * integer wraparound behavior. FIXME: remove this hack, which is probably not
 * robust anyway if C is aggressive about inlining.
 */
static int _add(int a, int b) {
  return a + b;
}
|]

includes :: [C.Definition]
includes = [cunit|
$esc:("#include \"ssm-program.h\"")
|]

-- | Generate definitions for an SSM 'Procedure'.
--
-- The fst element of the returned tuple contains the struct definition and
-- function prototype declarations, while the snd element contains the function
-- definitions.
genProcedure :: Procedure -> ([C.Definition], [C.Definition])
genProcedure p = runTR p $ do
  (stepDecl , stepDefn ) <- genStep
  (enterDecl, enterDefn) <- genEnter
  structDefn             <- genStruct
  return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])

-- | Generate struct definition for an SSM 'Procedure', where local variables,
-- triggers, and parameter values are stored.
genStruct :: TR C.Definition
genStruct = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  return [cedecl|
    typedef struct {
      struct act act;

      $sdecls:(map param (arguments p))
      $sdecls:(map local ls)
      $sdecls:(map trig [1..ts])

    } $id:(act_ $ identName $ name p);
  |]
 where
  param r
    | isReference (refType r) = [csdecl|$ty:(svt_ (refType r)) *$id:(refName r);|]
    | otherwise               = [csdecl|$ty:(svt_ (refType r)) $id:(refName r);|]

  local (id, t) = [csdecl|$ty:(svt_ t) $id:(identName id);|]

  trig i = [csdecl|$ty:trigger_t $id:t;|] where t = "trig" ++ show i

-- | Generate the enter function for an SSM 'Procedure', where its struct is
-- allocated and initialized (partially; local variables' values are left
-- uninitialized).
genEnter :: TR (C.Definition, C.Definition)
genEnter = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  let actname = identName $ name p
      act     = [cty|typename $id:act'|]
      act'    = act_ actname -- hack to use this typename as expr in macros
      enter   = enter_ actname
      step    = step_ actname
      params  =
        [cparams|$ty:act_t *caller, $ty:priority_t priority, $ty:depth_t depth|]
          ++ map param (arguments p)
  return
    ( [cedecl|$ty:act_t *$id:enter($params:params);|]
    , [cedecl|
        $ty:act_t *$id:enter($params:params) {
          $ty:act_t *actg = $id:act_enter(sizeof($ty:act), $id:step, caller, priority, depth);
          $ty:act *acts = container_of(actg, $id:act', act);

          /* Initialize and assign parameters */
          $stms:(concatMap initParam (arguments p))

          /* Initialize locals */
          $stms:(concat $ map initLocal ls)

          /* Initialize triggers */
          $stms:(map initTrig [1..ts])

          return actg;
        }
      |]
    )
 where
  -- | TODO: This only works because we don't have nested Refs (yet)
  param r
    | isReference (refType r) = [cparam|$ty:(svt_ (refType r)) *$id:(refName r)|]
    | otherwise = [cparam|typename $id:(typeId (refType r)) $id:(refName r)|]

  initParam r
    | isReference (refType r) = [[cstm|acts->$id:(refName r) = $id:(refName r);|]]
    | otherwise               =
      [ [cstm|$id:(initialize_ (refType r))(&acts->$id:(refName r));|]
      , [cstm|acts->$id:(refName r).value = $id:(refName r);|]
      ]

  initLocal (n, t) =
    [ [cstm| $id:(initialize_ t)(&acts->$id:(identName n));|]
    , [cstm| DEBUG_SV_SET_VAR_NAME(acts->$id:(identName n).sv.debug, $string:(identName n)); |]
    ]

  initTrig i = [cstm| acts->$id:trig.act = actg;|]
    where trig = "trig" ++ show i

-- | Generate the step function for an SSM 'Procedure'.
--
-- This function just defines the function definition and switch statement that
-- wraps the statements of the procedure. The heavy lifting is performed by
-- 'genCase'.
genStep :: TR (C.Definition, C.Definition)
genStep = do
  p     <- gets procedure
  _     <- nextCase -- Toss away 0th case
  cases <- concat <$> mapM genCase (body p)
  refs  <- gets locals
  final <- nextCase
  let actname = identName $ name p
      act     = [cty|typename $id:act'|]
      act'    = act_ $ actname -- Hack to use this typename as expr in macros
      step    = step_ $ actname
  return
    ( [cedecl|void $id:step($ty:act_t *actg);|]
    , [cedecl|
        void $id:step($ty:act_t *actg) {
          $ty:act *acts = container_of(actg, $id:act', act);
          switch (actg->pc) {
          case 0:;
            $stms:cases

          default:
            break;
          }
          $stms:(map dequeue refs)
          $id:act_leave(actg, sizeof($ty:act));
        }
      |]
    )
  where dequeue r = [cstm|$id:unsched_event(&acts->$id:(refName r).sv);|]

-- | Generate the list of statements from each 'Stm' in an SSM 'Procedure'.
--
-- Note that this compilation scheme might not work if the language were to
-- support return statements. This could be fixed by generating a break, and
-- moving the leave call to outside of the switch statement in 'genStep'.
--
-- TODOs: remove hard-coded act variable name.
genCase :: Stm -> TR [C.Stm]
genCase (NewRef n t v) = do
  locs <- map fst <$> gets locals
  let lvar = identName n
      lhs  = [cexp|&acts->$id:lvar|]
      rhs  = genExp locs v
  addLocal n t
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (GetRef n t (rvar, _)) = do
  locs <- map fst <$> gets locals
  let lvar = identName n
      lhs  = [cexp|&acts->$id:lvar|]
      rhs  = if rvar `elem` locs
        then [cexp|acts->$id:(identName rvar).value|]
        else [cexp|acts->$id:(identName rvar)->value|]
  addLocal n t
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (SetRef (lvar, t) e) = do
  locs <- map fst <$> gets locals
  let lhs = if lvar `elem` locs
        then [cexp|&acts->$id:(identName lvar)|]
        else [cexp|acts->$id:(identName lvar)|]
      rhs = genExp locs e
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (SetLocal n t e) = do
  locs <- map fst <$> gets locals
  let lvar = identName n
      lhs  = [cexp|&acts->$id:lvar|]
      rhs  = genExp locs e
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (If c t e) = do
  locs <- map fst <$> gets locals
  let cnd = genExp locs c
  thn <- concat <$> mapM genCase t
  els <- concat <$> mapM genCase e
  return [[cstm| if ($exp:cnd) { $stms:thn } else { $stms:els }|]]
genCase (While c b) = do
  locs <- map fst <$> gets locals
  let cnd = genExp locs c
  bod <- concat <$> mapM genCase b
  return [[cstm| while ($exp:cnd) { $stms:bod } |]]
genCase (After d (lvar, t) v) = do
  locs <- map fst <$> gets locals
  let del = genExp locs d
      lhs = if lvar `elem` locs
        then [cexp|&acts->$id:(identName lvar)|]
        else [cexp|acts->$id:(identName lvar)|]
      rhs = genExp locs v
      -- | NOTE: we add `now` to the delay here.
  return [[cstm| $id:(later_ t)($exp:lhs, now + $exp:del, $exp:rhs);|]]
genCase (Wait ts) = do
  caseNum <- nextCase
  maxWaits $ length ts
  locs <- map fst <$> gets locals
  let trigs = zip [1 ..] $ map (genTrig locs) ts
  return
    $  fmap sensitizeTrig trigs
    ++ [ [cstm| actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
    ++ fmap desensitizeTrig trigs
 where
  sensitizeTrig (i, trig) =
    [cstm|$id:sensitize($exp:trig, &acts->$id:(trig_ i));|]
  desensitizeTrig (i, _) = [cstm|$id:desensitize(&acts->$id:(trig_ i));|]
  genTrig locs (trig, _) = if trig `elem` locs
    then [cexp|&acts->$id:(identName trig).sv|]
    else [cexp|&acts->$id:(identName trig)->sv|]
genCase (Fork cs) = do
  locs    <- map fst <$> gets locals
  caseNum <- nextCase
  let
    genCall i (r, as) = [cstm|$id:fork($id:(enter_ (identName r))($args:enterArgs));|]
     where
      enterArgs =
        [ [cexp|actg|]
          , [cexp|actg->priority + $int:i * (1 << $exp:newDepth)|]
          , newDepth
          ]
          ++ map genArg as
      genArg (Left e) = genExp locs e
      genArg (Right (r, _)) =
        if r `elem` locs
          then [cexp|&acts->$id:(identName r)|]
          else [cexp|acts->$id:(identName r)|]

      newDepth = [cexp|actg->depth - $int:depthSub|]
      depthSub =
        (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Int

    genDebug (r, _) = r
  return
    $ [cstm| DEBUG_PRINT($string:((++ "\n") $ unwords $ "fork" : map (identName . fst) cs)); |]
    : zipWith genCall [0 :: Int ..] cs
    ++ [ [cstm| actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
genCase Skip = return []
