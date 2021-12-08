-- | Translate SSM program to C compilation unit.
--
-- Each procedure in a program is turned into three components:
--
-- 1) A struct (the activation record)
-- 2) An initialization function (the enter function)
-- 3) A step function, which corresponds to the actual procedure body

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
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

import           Data.List                      ( sortOn )
import           SSM.Backend.C.Identifiers
import           SSM.Backend.C.Types
import           SSM.Backend.C.Peripheral

import           SSM.Core

import qualified SSM.Interpret.Trace           as T

-- | Given a 'Program', returns a tuple containing the compiled program and
-- a list of all `include` statements.
compile_ :: Program C -> ([C.Definition], [C.Definition])
compile_ program = (compUnit, includes)
 where
  -- | The file to generate, minus include statements
  compUnit :: [C.Definition]
  compUnit = concat [ declarePeripherals program
                    , preamble
                    , decls
                    , defns
                    , initProg
                    ]

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

-- | Generate the entry point of a program - the first thing to be ran.
genInitProgram :: Program C -> [C.Definition]
genInitProgram p = [cunit|
  int $id:initialize_program(void) {
    $items:(initPeripherals p)
    $items:(initialForks $ initialQueueContent p)

    return 0;
  }
  |]
 where
  -- | Create statements for scheduling the initial ready-queue content
  initialForks :: [QueueContent C] -> [C.BlockItem]
  initialForks ips =
    zipWith
      initialFork
        (pdeps
          (length ips)
          priority_at_root
          depth_at_root)
        ips
    where
      -- | Create the schedule statement for a single schedulable thing
      initialFork :: (C.Exp, C.Exp) -> QueueContent C -> C.BlockItem
      initialFork (priority, depth) (SSMProcedure id args) =
        [citem| $id:fork($id:(enter_ (identName id))( &$id:top_parent
                                                    , $exp:priority
                                                    , $exp:depth
                                                    , $args:(map cargs args)
                                                    )
                        ); |]
      -- initialFork (priority, depth) (Handler h) =
      --   [citem|$id:fork($id:(resolveNameOfHandler h)
      --                                     ( &$id:top_parent
      --                                     , $exp:priority
      --                                     , $exp:depth
      --                                     , $args:(argsOfHandler h)
      --                                     )
      --                  );|]
      initialFork (priority, depth) (Handler f) = error "fixme"

      -- | Take a handler and return a list of arguments to it
      argsOfHandler :: Handler -> [C.Exp]
      argsOfHandler (Output variant ref) = case variant of
        LED id -> [ [cexp| &$id:(refName ref).sv |]
                  , [cexp| $uint:id |]
                  ]
        BLE bh -> case bh of
          Broadcast          -> [ [cexp| &$id:(refName ref).sv |] ]
          BroadcastControl   -> [ [cexp| &$id:(refName ref).sv |] ]
          ScanControl        -> [ [cexp| &$id:(refName ref).sv |] ]

      cargs :: Either SSMExp Reference -> C.Exp
      cargs (Left e)  = genExp [] e
      cargs (Right r@(Static _)) = [cexp| &$id:(refName r).sv|]
      cargs (Right r@(Dynamic _)) = error "Why does StaticOutputHandler refer to a non-static var?"
      x = refName

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
$esc:("#ifndef SSM_DEBUG_TRACE")
$esc:("#define SSM_DEBUG_TRACE(...) do ; while (0)")
$esc:("#endif")
$esc:("#ifndef SSM_DEBUG_MICROTICK")
$esc:("#define SSM_DEBUG_MICROTICK(...) do ; while (0)")
$esc:("#endif")
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
  return [[cstm| if ($exp:cnd) $stm:(wrapBranch thn) else $stm:(wrapBranch els)|]]
  where
    wrapBranch :: [C.Stm] -> C.Stm
    wrapBranch []  = [cstm| {} |]
    wrapBranch [x] = [cstm| $stm:x |]
    wrapBranch stms@(x:y:xs) = [cstm| { $stms:stms; } |]
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
    pds = pdeps (length cs) [cexp|actg->priority|] [cexp|actg->depth|]

    genCall :: (C.Exp, C.Exp) -> (Ident, [Either SSMExp Reference]) -> C.Stm
    genCall (prio, depth) (r, as) =
      [cstm|$id:fork($id:(enter_ (identName r))($args:enterArgs));|]
     where
      enterArgs =
        [ [cexp|actg|]
          , prio
          , depth
          ]
          ++ map genArg as
      genArg :: Either SSMExp Reference -> C.Exp
      genArg (Left  e) = genExp locs e
      genArg (Right r) = refPtr r locs

    -- | Generate a test that raises an exception if the program has run out of prioities
    checkNewDepth :: C.Stm
    checkNewDepth = [cstm|
      if ($id:actg->depth < $exp:(depthSub (length cs)))
         $id:throw($exp:exhausted_priority); |]

    -- | Generate the trace item that advertises which processes are being forked
    genTrace :: (Ident, [Either SSMExp Reference]) -> C.Stm
    genTrace (r, _) = [cstm|$id:debug_trace($string:event);|]
      where event = show $ T.ActActivate $ identName r

  return
    $  checkNewDepth
    :  map genTrace cs
    ++ zipWith genCall pds cs
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
genExp _  (Lit _ (LUInt32 i    )) = [cexp|$uint:i|]
genExp _  (Lit _ (LInt64  i    )) = [cexp|$lint:i|]
genExp _  (Lit _ (LUInt64 i    )) = [cexp|$ulint:i|]
genExp _  (Lit _ (LBool   True )) = [cexp|true|]
genExp _  (Lit _ (LBool   False)) = [cexp|false|]
genExp _  (Lit _ (LEvent       )) = [cexp|0|]
genExp ls (UOpE _ e op)           = case op of
  Neg -> [cexp|- $exp:(genExp ls e)|]
  Not -> [cexp|! $exp:(genExp ls e)|]
genExp ls (UOpR t r op) = case op of
  Changed -> [cexp|$id:event_on(&$exp:(refPtr r ls)->sv)|]
  Deref   -> case t of
    TEvent -> [cexp|0|]
    _      -> [cexp|$exp:(refPtr r ls)->value|]
genExp ls (BOp ty e1 e2 op) = gen op
 where
  (c1, c2) = (genExp ls e1, genExp ls e2)
  (s1, s2) = (signed_ (expType e1) c1, signed_ (expType e2) c2)
  gen OPlus   = [cexp|$exp:c1 + $exp:c2|]
  gen OMinus  = [cexp|$exp:c1 - $exp:c2|]
  gen OTimes  = [cexp|$exp:c1 * $exp:c2|]
  gen ODiv    = [cexp|$exp:c1 / $exp:c2|]
  gen ORem    = [cexp|$exp:s1 % $exp:s2|]
  gen OMin    = [cexp|$exp:ltcomparison ? $exp:c1 : $exp:c2|]
  gen OMax    = [cexp|$exp:ltcomparison ? $exp:c2 : $exp:c1|]
  gen OLT     = ltcomparison
  gen OEQ     = [cexp|$exp:c1 == $exp:c2|]
  gen OAnd    = [cexp|$exp:c1 && $exp:c2|]
  gen OOr     = [cexp|$exp:c1 || $exp:c2|]
  gen OLShift = [cexp|$exp:c1 << $exp:c2|]
  gen ORShift = [cexp|$exp:c1 >> $exp:c2|]
  gen OBAnd   = [cexp|$exp:c1 & $exp:c2|]
  gen OBOr    = [cexp|$exp:c1 | $exp:c2|]
  gen OBXor   = [cexp|$exp:c1 ^ $exp:c2|]

  ltcomparison :: C.Exp
  ltcomparison = [cexp| $exp:lhs < $exp:rhs |]
    where
      lhs = [cexp| $exp:s1 |]
      rhs = [cexp| $exp:s2 |]

genTimeDelay :: [Reference] -> SSMExp -> C.Exp
genTimeDelay ls d = [cexp|$exp:(genExp ls d)|]

-- | Promote a C expression to a C statement.
expToStm :: C.Exp -> C.Stm
expToStm e = [cstm|$exp:e;|]

-- | Promote a C statement to a C block item.
stmToItem :: C.Stm -> C.BlockItem
stmToItem s = [citem|$stm:s|]
