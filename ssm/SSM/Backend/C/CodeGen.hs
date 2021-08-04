-- | Translate SSM program to C compilation unit.
--
-- Each procedure in a program is turned into three components:
--
-- 1) A struct (the activation record)
-- 2) An initialization function (the enter function)
-- 3) A step function, which corresponds to the actual procedure body

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SSM.Backend.C.CodeGen
  ( compile_
  ) where

import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Data.Either                    ( rights )
import qualified Data.Map                      as Map

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

-- import           Data.Bifunctor                 ( second )
import           Data.List                      ( sortOn )
import           SSM.Backend.C.Exp
import           SSM.Backend.C.Identifiers
import           SSM.Backend.C.Types
import           SSM.Core.Syntax
import qualified SSM.Interpret.Trace           as T
import Debug.Trace
-- TODOs: remove hard-coded identifiers.

-- | Given a 'Program', returns a tuple containing the compiled program and
-- a list of all `include` statements.
compile_ :: Program -> ([C.Definition], [C.Definition])
compile_ program = (compUnit, includes)
 where
  compUnit = globals ++ preamble ++ decls ++ defns ++ entryPointSymbol

  globals = genGlobals (global_references program)

  preamble = genPreamble
  (decls, defns) =
    concat2 $ unzip $ map genProcedure $ Map.elems (funs program)
  concat2 (x, y) = (concat x, concat y)
  entryPointSymbol = [cunit|
    $ty:act_t *(*$id:entry_point)($ty:act_t *, $ty:priority_t, $ty:depth_t) =
      $id:(enter_ $ identName $ entry program);
  |]

{- | State maintained while compiling a 'Procedure'.

The information here is populated while generating the step function, so that
should be computed first, before this information is used to generate the act
struct and enter definitions.
-}
data TRState = TRState
  { procedure :: Procedure    -- ^ Procedure we are compiling
  , ncase     :: Int          -- ^ Which number has the next case?
  , numwaits  :: Int          -- ^ The size of the widest wait
  , locals    :: [Reference]  -- ^ Local references declared with var
  }

-- | Translation monad.
type TR a = State TRState a

-- | Run a TR computation on a procedure.
runTR :: Procedure -> TR a -> a
runTR p tra = evalState tra $ TRState p 0 0 []

-- | Read and increment the number of cases in a procedure, i.e., `ncase++`.
nextCase :: TR Int
nextCase = do
  n <- gets ncase
  modify $ \st -> st { ncase = n + 1 }
  return n

-- | Register a local variable for which an sv should be allocated.
addLocal :: Reference -> TR ()
addLocal r = modify $ \st -> st { locals = r : locals st }

-- | Maintain the maximum number of variables a 'Procedure' waits on.
maxWaits :: Int -> TR ()
maxWaits rs = modify $ \st -> st { numwaits = rs `max` numwaits st }

{-------- Code generation --------}

-- | Identifier for generic (inner) struct act.
actg :: CIdent
actg = "actg"

-- | Identifier for specialized (outer) struct act.
acts :: CIdent
acts = "acts"

-- | Identifier for act member in act struct.
actm :: CIdent
actm = "act"
{- | Generate the declarations of global variables and the function that initializes
them. These variables can be accessed without an activation record. -}
genGlobals :: [(Ident, Type)] -> [C.Definition]
genGlobals globals = globalvars ++ [initglobals]
  where
    -- | The global variable declarations
    globalvars :: [C.Definition]
    globalvars = map (\(n,t) -> [cedecl| $ty:(svt_ t) $id:(identName n); |]) globals

    -- | The function which initializes them
    initglobals :: C.Definition
    initglobals = [cedecl| void initialize_global_variables() { $items:stmts } |]
      where
        -- | The statements that initializes the variables
        stmts :: [C.BlockItem]
        stmts = map (\(n,t) -> [citem| $id:(initialize_ t)(&$id:(identName n)); |]) globals

-- | Generate include statements, to be placed at the top of the generated C.
genPreamble :: [C.Definition]
genPreamble = [cunit|

/**
 * FIXME: Generate typedefs, to be placed at the top of the generated C.
 * Typedef 'event' type as a dummy type. This is because the C runtime does not
 * define ssm_event_t to wrap a value like the other sv's do. The edsl uses ()
 * as that wrapee type, effectively making ssm_event_t a unit type.
 *
 * Note that variables of this type will be ignored when 'assigning' to
 * ssm_event_t's. They are simply used so there is a 'primitive' counterpart
 * to ssm_event_t's. This hack will also allow for programs that test the
 * equality of event types to compile while we flesh out the exact semantics of
 * such an operation. In the meantime, all event 'literals' are just chars with
 * value 0.
 */
typedef char event;

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
$esc:("#include \"ssm-platform.h\"")
|]

{- | Generate definitions for an SSM 'Procedure'.

The fst element of the returned tuple contains the struct definition and
function prototype declarations, while the snd element contains the function
definitions.
-}
genProcedure :: Procedure -> ([C.Definition], [C.Definition])
genProcedure p = runTR p $ do
  (stepDecl , stepDefn ) <- genStep
  (enterDecl, enterDefn) <- genEnter
  structDefn             <- genStruct
  return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])

{- | Generate struct definition for an SSM 'Procedure'.

This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: TR C.Definition
genStruct = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  return [cedecl|
    typedef struct {
      $ty:act_t $id:actm;

      $sdecls:(map param (arguments p))
      $sdecls:(map local ls)
      $sdecls:(map trig [1..ts])

    } $id:(act_ $ identName $ name p);
  |]
 where
  param (n,t)
    | isReference t = [csdecl|$ty:(svt_ t) *$id:(identName n);|]
    | otherwise     = [csdecl|$ty:(svt_ t) $id:(identName n);|]

  local ref = [csdecl|$ty:(svt_ (refType ref)) $id:(refName ref);|]

  trig i = [csdecl|$ty:trigger_t $id:t;|] where t = "trig" ++ show i

{- | Generate the enter function for an SSM 'Procedure'.

Its struct is allocated and initialized (partially; local variables' values are
left uninitialized).
-}
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
          $ty:act_t *$id:actg = $id:act_enter(sizeof($ty:act), $id:step, caller, priority, depth);
          $ty:act *$id:acts = container_of($id:actg, $id:act', act);

          /* Initialize and assign parameters */
          $stms:(concatMap initParam (arguments p))

          /* Initialize locals */
          $stms:(concat $ map initLocal ls)

          /* Initialize triggers */
          $stms:(map initTrig [1..ts])

          return $id:actg;
        }
      |]
    )
 where
  -- | TODO: This only works because we don't have nested Refs (yet)
  param (n,t)
    | isReference t = [cparam|$ty:(svt_ t) *$id:(identName n)|]
    | otherwise     = [cparam|$ty:(basetype t) $id:(identName n)|]

  initParam (n,t)
    | isReference t = [[cstm|acts->$id:(identName n) = $id:(identName n);|]]
    | baseType t == TEvent =
      [ [cstm|$id:(initialize_ t)(&$id:acts->$id:(identName n));|],
        [cstm|$id:(assign_ t)(&$id:acts->$id:(identName n), $id:actg->priority);|]]
    | otherwise               =
      [ [cstm|$id:(initialize_ t)(&acts->$id:(identName n));|]
      , [cstm|acts->$id:(identName n).value = $id:(identName n);|]
      ]

  initLocal ref =
    [ [cstm| $id:(initialize_ (refType ref))(&acts->$id:(refName ref));|] ]

  initTrig i = [cstm| $id:acts->$id:trig.act = $id:actg;|]
    where trig = "trig" ++ show i

{- | Generate the step function for an SSM 'Procedure'.

This function just defines the function definition and switch statement that
wraps the statements of the procedure. The heavy lifting is performed by
'genCase'.
-}
genStep :: TR (C.Definition, C.Definition)
genStep = do
  p     <- gets procedure
  _     <- nextCase -- Toss away 0th case
  cases <- concat <$> mapM genCase (body p)
  locs  <- gets locals
  final <- nextCase
  let
    actname       = identName $ name p
    act           = [cty|typename $id:actt|]
    actt          = act_ actname -- hack to use this typename as expr in macros
    step          = step_ actname

    actStepBeginS = show $ T.ActStepBegin $ actname
    actLocalVarS nt = show $ T.ActVar $ varFmt nt

    debugLocal r
      | baseType (refType r) == TEvent
      = [cstm|if ($exp:initialized) $id:debug_trace($exp:fmt);|]
      | otherwise
      = [cstm|if ($exp:initialized) $id:debug_trace($exp:fmt, $exp:val);|]
     where
      initialized = [cexp|$id:acts->$id:(refName r).sv.last_updated != $id:never|]
      fmt         = [cexp|$string:(actLocalVarS (refIdent r, refType r))|]
      val         = [cexp|$id:acts->$id:(refName r).value|]

    debugArg (n,t) | baseType t == TEvent = [cstm|$id:debug_trace($exp:fmt);|]
                   | otherwise            = [cstm|$id:debug_trace($exp:fmt, $exp:val);|]
     where
      fmt = [cexp|$string:(actLocalVarS (n, t))|]
      val = case t of
        Ref _ -> [cexp|$id:acts->$id:(identName n)->value|]
        _     -> [cexp|$id:acts->$id:(identName n).value|]

    dequeue r = [cstm|$id:unsched_event(&$id:acts->$id:(refName r).sv);|]
  return
    ( [cedecl|void $id:step($ty:act_t *$id:actg);|]
    , [cedecl|
        void $id:step($ty:act_t *$id:actg) {
          $ty:act *$id:acts = container_of($id:actg, $id:actt, act);

          $id:debug_trace($string:actStepBeginS);
          $stms:(map debugArg $ sortOn fst $ arguments p)
          $stms:(map debugLocal $ sortOn refIdent locs)
          $id:debug_microtick();

          switch ($id:actg->pc) {
          case 0:;
            $stms:cases

          default:
            break;
          }
          $stms:(map dequeue locs)
          $id:act_leave($id:actg, sizeof($ty:act));
        }
      |]
    )

{- | Generate the list of statements from each 'Stm' in an SSM 'Procedure'.

Note that this compilation scheme might not work if the language were to
support return statements. This could be fixed by generating a break, and
moving the leave call to outside of the switch statement in 'genStep'.
-}
genCase :: Stm -> TR [C.Stm]
genCase (NewRef n t v) = do
  locs <- gets locals
  let lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs  = genExp locs v
  addLocal $ makeDynamicRef n (mkReference t)
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (GetRef n t r) = do
  locs <- gets locals
  let rvar = refIdent r
      lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs = refVal r locs
  addLocal $ makeDynamicRef n t
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (SetRef r e) = do
  locs <- gets locals
  let lvar = refIdent r
      t    = refType r
      lhs  = refPtr r locs
      rhs = genExp locs e
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (SetLocal n t e) = do
  locs <- gets locals
  let lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs  = genExp locs e
  case baseType t of
    TEvent -> return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority);|]]
    _ ->
      return [[cstm|$id:(assign_ t)($exp:lhs, $id:actg->priority, $exp:rhs);|]]
genCase (If c t e) = do
  locs <- gets locals
  let cnd = genExp locs c
  thn <- concat <$> mapM genCase t
  els <- concat <$> mapM genCase e
  return [[cstm| if ($exp:cnd) { $stms:thn } else { $stms:els }|]]
genCase (While c b) = do
  locs <- gets locals
  let cnd = genExp locs c
  bod <- concat <$> mapM genCase b
  return [[cstm| while ($exp:cnd) { $id:debug_microtick(); $stms:bod } |]]
genCase (After d r v) = do
  locs <- gets locals
  let lvar = refIdent r
      t    = refType r
      del  = genExp locs d
      lhs  = refPtr r locs
      rhs = genExp locs v
  -- Note that the semantics of 'After' and 'later_' differ---the former
  -- expects a relative time, whereas the latter takes an absolute time.
  -- Thus we add now() in the code we generate.
  case baseType t of
    TEvent -> return [[cstm| $id:(later_ t)($exp:lhs, $id:now() + $exp:del);|]]
    _      -> return
      [[cstm| $id:(later_ t)($exp:lhs, $id:now() + $exp:del, $exp:rhs);|]]
genCase (Wait ts) = do
  caseNum <- nextCase
  maxWaits $ length ts
  locs <- gets locals

--  traceM $ "\n\n"
--  traceM $ show locs
--  traceM $ "\n\n"

  let trigs = zip [1 ..] $ map (flip refSV locs) ts
  return
    $ map getTrace ts
    ++ map sensitizeTrig trigs
    ++ [ [cstm| $id:actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
    ++ fmap desensitizeTrig trigs
 where
  sensitizeTrig (i, trig) = [cstm|$id:sensitize($exp:trig, &$id:acts->$id:(trig_ i));|]
  desensitizeTrig (i,_) = [cstm|$id:desensitize(&$id:acts->$id:(trig_ i));|]
--  genTrig locs r | refIdent r `elem` locs = [cexp|&$id:acts->$id:(refName r).sv|]
--                 | otherwise              = [cexp|&$id:acts->$id:(refName r)->sv|]
  getTrace r = [cstm|$id:debug_trace($string:event);|]
    where event = show $ T.ActSensitize $ refName r
genCase (Fork cs) = do
  locs    <- gets locals
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
      genArg :: Either SSMExp Reference -> C.Exp
      genArg (Left e) = genExp locs e
      genArg (Right r) = refPtr r locs

    newDepth = [cexp|actg->depth - $int:depthSub|]
    depthSub =
      (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Int

    checkNewDepth = [cstm|
      if ($id:actg->depth < $int:depthSub)
         $id:throw($exp:exhausted_priority); |]

    genTrace (r, _) = [cstm|$id:debug_trace($string:event);|]
      where event = show $ T.ActActivate $ identName r

  return
    $  checkNewDepth
    :  map genTrace cs
    ++ zipWith genCall [0 :: Int ..] cs
    ++ [ [cstm| $id:actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
genCase Skip = return []
