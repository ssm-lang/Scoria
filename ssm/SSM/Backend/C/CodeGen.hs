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
import           SSM.Backend.C.Identifiers
import           SSM.Backend.C.Types
import           SSM.Backend.C.Peripheral
import           SSM.Core.Syntax
import           SSM.Core.Peripheral.GPIO
import qualified SSM.Interpret.Trace           as T

-- | Given a 'Program', returns a tuple containing the compiled program and
-- a list of all `include` statements.
compile_ :: Program -> ([C.Definition], [C.Definition])
compile_ program = (compUnit, includes)
 where
  -- | The file to generate, minus include statements
  compUnit :: [C.Definition]
  compUnit = concat [ globals
                    , declarePeripherals program
                    , preamble
                    , decls
                    , defns
                    , initProg
                    ]

  -- | Global reference declarations
  globals :: [C.Definition]
  globals = genGlobals program

  initProg :: [C.Definition]
  initProg = genInitProgram program

  -- | Preamble, macros etc
  preamble :: [C.Definition]
  preamble = genPreamble

  -- | declarations and definitions, prototypes etc
  decls, defns :: [C.Definition]
  (decls, defns) =
    concat2 $ unzip $ map genProcedure $ Map.elems (funs program)

  -- | Utility function to distribute @concat@ over a tuple
  concat2 :: ([[a]], [[b]]) -> ([a], [b])
  concat2 (x, y) = (concat x, concat y)

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

-- TODOs: remove hard-coded identifiers like 'caller', 'priority', 'value', etc.

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
genGlobals :: Program -> [C.Definition]
genGlobals = map declGlobal . globalReferences
 where
  -- | Declare global variable.
  declGlobal :: (Ident, Type) -> C.Definition
  declGlobal (n, t) = [cedecl|$ty:(svt_ $ stripRef t) $id:(identName n);|]

--    $id:fork($id:(enter_ $ identName $ entry p)
--        (&ssm_top_parent, SSM_ROOT_PRIORITY, SSM_ROOT_DEPTH));

-- | Generate the entry point of a program - the first thing to be ran.
genInitProgram :: Program -> [C.Definition]
genInitProgram p = [cunit|
  void $id:initialize_program(void) {
    $items:(map initGlobal $ globalReferences p)
    $items:(initPeripherals p)
    $items:(initialForks $ initialQueueContent p)
  }
  |]
 where
  -- | Initialize global reference.
  initGlobal :: (Ident, Type) -> C.BlockItem
  initGlobal (n, t) = [citem|$exp:init;|]
    where init = initialize_ (stripRef t) [cexp|&$id:(identName n)|]

  initialForks :: [QueueContent] -> [C.BlockItem]
  initialForks ips = map (uncurry initialFork) $ zip (pdeps (length ips)) ips
    where
      initialFork :: (C.Exp, C.Exp) -> QueueContent -> C.BlockItem
      initialFork (priority, depth) (SSMProcedure id args) =
        [citem| $id:fork($id:(enter_ (identName id))( &$id:top_parent
                                                    , $exp:priority
                                                    , $exp:depth
                                                    , $args:(map cargs args)
                                                    )
                        ); |]
      initialFork (priority, depth) (Handler h) =
        [citem|$id:fork($id:(resolveNameOfHandler h)
                                          ( &$id:top_parent
                                          , $exp:priority
                                          , $exp:depth
                                          , $args:(map cargs $ argsOfHandler h)
                                          )
                       );|]

      argsOfHandler :: Handler -> [Either SSMExp Reference]
      argsOfHandler (StaticOutputHandler ref id) =
        [ Right ref
        , Left $ Lit TUInt8 $ LUInt8 id
        ]

      pdeps :: Int -> [(C.Exp, C.Exp)]
      pdeps cs =
        let depthsub = ceiling $ logBase (2 :: Double) $ fromIntegral $ cs :: Int
        in [ let prio  = [cexp|PRIORITY_AT_ROOT + ($int:(i-1) * (1 << $exp:depth))|]
                 depth = [cexp|DEPTH_AT_ROOT - $int:depthsub|]
             in (prio, depth)
           | i <- [1..cs]
           ]

      cargs :: Either SSMExp Reference -> C.Exp
      cargs (Left e)  = genExp [] e
      cargs (Right r) = refPtr r []

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
typedef char $id:event;
|]

-- | Include statements in the generated file
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
  -- | Turn a @(Ident, Type)@ pair into a C function parameter
  param :: (Ident, Type) -> C.FieldGroup
  param (n, t)
    | isReference t = [csdecl|$ty:(svt_ $ stripRef t) *$id:(identName n);|]
    | otherwise     = [csdecl|$ty:(base_ t) $id:(identName n);|]

  {- | Return a scheduled variable field, of the same type and name as the
  argument reference. -}
  local :: Reference -> C.FieldGroup
  local ref = [csdecl|$ty:(svt_ $ stripRef $ refType ref) $id:(refName ref);|]

  -- | Return a trigger field, identified by the @Int@ argument
  trig :: Int -> C.FieldGroup
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
      params =
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
  param :: (Ident, Type) -> C.Param
  param (n, t)
    | isReference t = [cparam|$ty:(svt_ $ stripRef t) *$id:(identName n)|]
    | otherwise     = [cparam|$ty:(base_ t) $id:(identName n)|]

  -- | initialize a parameter
  initParam :: (Ident, Type) -> [C.Stm]
  initParam (n, t) = [[cstm|$id:acts->$id:(identName n) = $id:(identName n);|]]

  -- | Initialize a local reference
  initLocal :: Reference -> [C.Stm]
  initLocal ref = [[cstm| $exp:(initialize_ t r);|]]
   where
    t = stripRef $ refType ref
    r = [cexp|&$id:acts->$id:(refName ref)|]

  -- | Initialize a trigger
  initTrig :: Int -> C.Stm
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
  let actname       = identName $ name p
      act           = [cty|typename $id:actt|]
      actt          = act_ actname -- hack to use this typename as expr in macros
      step          = step_ actname

      actStepBeginS = show $ T.ActStepBegin actname

      debugLocal :: Reference -> C.Stm
      debugLocal r =
        [cstm|if ($exp:last_updated != $id:never) $exp:(trace_ ty name val);|]
       where
        last_updated = [cexp|$id:acts->$id:(refName r).sv.last_updated|]
        (ty, name)   = (stripRef $ refType r, refIdent r)
        val          = [cexp|$id:acts->$id:(refName r).value|]

      debugArg :: (Ident, Type) -> C.Exp
      debugArg (n, t)
        | isReference t = trace_ (stripRef t)
                                 n
                                 [cexp|$id:acts->$id:(identName n)->value|]
        | otherwise = trace_ t n [cexp|$id:acts->$id:(identName n)|]

      -- | Dequeue any outstanding event on a reference
      dequeue :: Reference -> C.Stm
      dequeue r = [cstm|$id:unsched_event(&$id:acts->$id:(refName r).sv);|]

  return
    ( [cedecl|void $id:step($ty:act_t *$id:actg);|]
    , [cedecl|
        void $id:step($ty:act_t *$id:actg) {
          $ty:act *$id:acts = container_of($id:actg, $id:actt, act);

          $id:debug_trace($string:actStepBeginS);
          $stms:(map (expToStm . debugArg) $ sortOn fst $ arguments p)
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
      prio = [cexp|$id:actg->priority|]
      rhs  = genExp locs v
  addLocal $ makeDynamicRef n (mkReference t)
  return [expToStm $ assign_ t lhs prio rhs]
genCase (SetRef r e) = do
  locs <- gets locals
  let lvar = refIdent r
      t    = stripRef $ refType r
      lhs  = refPtr r locs
      prio = [cexp|$id:actg->priority|]
      rhs  = genExp locs e
  return [expToStm $ assign_ t lhs prio rhs]
genCase (SetLocal n t e) = do
  locs <- gets locals
  let lvar = identName n
      lhs  = [cexp|&$id:acts->$id:lvar|]
      rhs  = genExp locs e
  return [[cstm| $id:acts->$id:(identName n) = $exp:rhs;|]]
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
      t    = stripRef $ refType r
      del  = genTimeDelay locs d
      lhs  = refPtr r locs
      -- Note that the semantics of 'After' and 'later_' differ---the former
      -- expects a relative time, whereas the latter takes an absolute time.
      -- Thus we add now() in the code we generate.
      time = [cexp|$id:now() + $exp:del|]
      rhs  = genExp locs v
  return [expToStm $ later_ t lhs time rhs]
genCase (Wait ts) = do
  caseNum <- nextCase
  maxWaits $ length ts
  locs <- gets locals
  let trigs = zip [1 ..] $ map (`refPtr` locs) ts
  return
    $  map getTrace      ts
    ++ map sensitizeTrig trigs
    ++ [ [cstm| $id:actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
    ++ fmap desensitizeTrig trigs
 where
  sensitizeTrig :: (Int, C.Exp) -> C.Stm
  sensitizeTrig (i, trig) =
    [cstm|$id:sensitize(&$exp:trig->sv, &$id:acts->$id:(trig_ i));|]

  desensitizeTrig :: (Int, C.Exp) -> C.Stm
  desensitizeTrig (i, _) = [cstm|$id:desensitize(&$id:acts->$id:(trig_ i));|]

  getTrace :: Reference -> C.Stm
  getTrace r = [cstm|$id:debug_trace($string:event);|]
    where event = show $ T.ActSensitize $ refName r
genCase (Fork cs) = do
  locs    <- gets locals
  caseNum <- nextCase
  let
    genCall :: Int -> (Ident, [Either SSMExp Reference]) -> C.Stm
    genCall i (r, as) =
      [cstm|$id:fork($id:(enter_ (identName r))($args:enterArgs));|]
     where
      enterArgs =
        [ [cexp|actg|]
          , [cexp|actg->priority + $int:i * (1 << $exp:newDepth)|]
          , newDepth
          ]
          ++ map genArg as
      genArg :: Either SSMExp Reference -> C.Exp
      genArg (Left  e) = genExp locs e
      genArg (Right r) = refPtr r locs

    newDepth :: C.Exp
    newDepth = [cexp|actg->depth - $int:depthSub|]

    depthSub :: Int
    depthSub =
      (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Int

    checkNewDepth :: C.Stm
    checkNewDepth = [cstm|
      if ($id:actg->depth < $int:depthSub)
         $id:throw($exp:exhausted_priority); |]

    genTrace :: (Ident, [Either SSMExp Reference]) -> C.Stm
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

-- | Produce a C expression that gives a pointer to an SV 'Reference'.
refPtr :: Reference -> [Reference] -> C.Exp
refPtr r@(Dynamic _) locs | r `elem` locs = [cexp|&$id:acts->$id:(refName r)|]
                          | otherwise     = [cexp|$id:acts->$id:(refName r)|]
refPtr r@(Static _) _ = [cexp|&$id:(refName r)|]

-- | Generate C expression from 'SSMExp' and a list of local variables.
genExp :: [Reference] -> SSMExp -> C.Exp
genExp _  (Var TEvent _         ) = [cexp|0|]
genExp _  (Var t n              ) = [cexp|acts->$id:(identName n)|]
genExp _  (Lit _ (LInt32  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LUInt8  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LInt64  i    )) = [cexp|(typename i64) $int:i|]
genExp _  (Lit _ (LUInt64 i    )) = [cexp|(typename u64) $int:i|]
genExp _  (Lit _ (LBool   True )) = [cexp|true|]
genExp _  (Lit _ (LBool   False)) = [cexp|false|]
genExp _  (Lit _ (LEvent       )) = [cexp|0|]
genExp ls (UOpE _ e op)           = case op of
  Neg -> [cexp|- $exp:(genExp ls e)|]
  Not -> [cexp|! $exp:(genExp ls e)|]
genExp ls (UOpR t r op) = case op of
  Changed -> [cexp|$id:event_on($exp:(refPtr r ls)->sv)|]
  Deref   -> case t of
    TEvent -> [cexp|0|]
    _      -> [cexp|$exp:(refPtr r ls)->value|]
-- | Circumvent optimizations that take advantage of C's undefined signed
-- integer wraparound behavior. FIXME: remove this hack, which is probably not
-- robust anyway if C is aggressive about inlining.
genExp ls (BOp ty e1 e2 op)
  | ty == TInt32 && op == OPlus = [cexp|_add($exp:c1, $exp:c2)|]
  | otherwise                   = gen op
 where
  (c1, c2) = (genExp ls e1, genExp ls e2)
  gen OPlus  = [cexp|$exp:c1 + $exp:c2|]
  gen OMinus = [cexp|$exp:c1 - $exp:c2|]
  gen OTimes = [cexp|$exp:c1 * $exp:c2|]
  gen ODiv   = [cexp|$exp:c1 / $exp:c2|]
  gen ORem   = [cexp|$exp:c1 % $exp:c2|]
  gen OMin   = [cexp|$exp:c1 < $exp:c2 ? $exp:c1 : $exp:c2|]
  gen OMax   = [cexp|$exp:c1 < $exp:c2 ? $exp:c2 : $exp:c1|]
  gen OLT    = [cexp|$exp:c1 < $exp:c2|]
  gen OEQ    = [cexp|$exp:c1 == $exp:c2|]
  gen OAnd   = [cexp|$exp:c1 && $exp:c2|]
  gen OOr    = [cexp|$exp:c1 || $exp:c2|]

genTimeDelay :: [Reference] -> SSMTime -> C.Exp
genTimeDelay ls (SSMTime d u) = [cexp|$exp:(genExp ls d) * $id:(units_ u)|]
genTimeDelay ls (SSMTimeAdd t1 t2) = let t1' = genTimeDelay ls t1
                                         t2' = genTimeDelay ls t2
                                     in [cexp|($exp:t1') + ($exp:t2')|]
genTimeDelay ls (SSMTimeSub t1 t2) = let t1' = genTimeDelay ls t1
                                         t2' = genTimeDelay ls t2
                                     in [cexp|($exp:t1') - ($exp:t2')|]
genTimeDelay ls (SSMTimeDiv t1 d)  = let t1' = genTimeDelay ls t1
                                     in [cexp|$exp:t1' / (typename u64) $exp:(genExp ls d)|]

-- | Promote a C expression to a C statement.
expToStm :: C.Exp -> C.Stm
expToStm e = [cstm|$exp:e;|]

-- | Promote a C statement to a C block item.
stmToItem :: C.Stm -> C.BlockItem
stmToItem s = [citem|$stm:s|]
