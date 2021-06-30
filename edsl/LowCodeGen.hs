{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module LowCodeGen where

import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Data.Either                    ( rights )
import qualified Data.Map                      as Map

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           LowCore

import           Debug.Trace
import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | This function takes a `Program` and returns a string, which contains the
-- pretty-printed content of the generated C file.
compile_
  :: Bool      -- ^ Whether to generate a main function
  -> Maybe Int -- ^ An optional tick limit (FIXME: apparently unused?)
  -> Program   -- ^ The program to be compiled
  -> String    -- ^ The pretty-printed content of the generated C file
compile_ m tl program = pretty 120 $ pprList compUnit
 where
  compUnit = preamble ++ decls ++ defns ++ if m then genMain program else []

  preamble = genPreamble tl
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
  , locals    :: [(String, Type)] -- ^ Local references declared with var
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
addLocal :: String -> Type -> TR ()
addLocal n t = modify $ \st -> st { locals = (n, t) : locals st }

-- | Maintain the maximum number of variables a 'Procedure' waits on.
maxWaits :: Int -> TR ()
maxWaits rs = modify $ \st -> st { numwaits = rs `max` numwaits st }

{-------- C identifiers --------}
-- These variables represent the magic identifiers that must be coordinated
-- between the C runtime and the generated code, as well as some helpers to
-- generate C type nodes for user-defined types.

-- | Type alias for C identifiers.
type CIdent = String

top_return :: CIdent
top_return = "top_return"

fork :: CIdent
fork = "act_fork"

act_enter :: CIdent
act_enter = "act_enter"

act_leave :: CIdent
act_leave = "act_leave"

event_on :: CIdent
event_on = "event_on"

sensitize :: CIdent
sensitize = "sensitize"

desensitize :: CIdent
desensitize = "desensitize"

unsched_event :: CIdent
unsched_event = "unsched_event"

time_t :: C.Type
time_t = [cty|typename ssm_time_t|]

priority_t :: C.Type
priority_t = [cty|typename priority_t|]

depth_t :: C.Type
depth_t = [cty|typename depth_t|]

trigger_t :: C.Type
trigger_t = [cty|struct trigger|]

stepf_t :: C.Type
stepf_t = [cty|typename stepf_t|]

uint16_t :: C.Type
uint16_t = [cty|typename uint16_t|]

bool_t :: C.Type
bool_t = [cty|typename bool|]

{---- Activation record identifiers ----}

-- | The type of the activation record base class.
act_t :: C.Type
act_t = [cty|struct act|]

-- | Obtain the name of the activation record struct for a routine.
act_ :: String -> CIdent
act_ routineName = "act_" ++ routineName ++ "_t"

-- | Obtain the name of the step function of a routine.
step_ :: String -> CIdent
step_ routineName = "step_" ++ routineName

-- | Obtain the name for the enter function of a routine.
enter_ :: String -> CIdent
enter_ routineName = "enter_" ++ routineName

-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "trig" ++ show i

{---- Type identifiers ----}

-- | The type of the scheduled variable base class.
sv_t :: C.Type
sv_t = [cty|struct sv|]

-- | Maps SSM `Type` to identifier of base type.
--
-- Note that this unwraps reference types and returns the base type.
typeId :: Type -> CIdent
typeId TInt32  = "i32"
typeId TInt64  = "i64"
typeId TUInt64 = "u64"
typeId TUInt8  = "u8"
typeId TBool   = "bool"
typeId (Ref t) = typeId t

-- | Obtain the name of the scheduled variable type for an SSM `Type`.
svt_ :: Type -> C.Type
svt_ ty = [cty|typename $id:(typeId ty ++ "_svt")|]

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: Type -> CIdent
initialize_ ty = "initialize_" ++ typeId ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: Type -> CIdent
assign_ ty = "assign_" ++ typeId ty

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: Type -> CIdent
later_ ty = "later_" ++ typeId ty

{-------- Code generation --------}

-- | Generate include statements, to be placed at the top of the generated C.
genPreamble :: Maybe Int -> [C.Definition]
genPreamble tickLimit = [cunit|
$esc:("#include \"ssm-program.h\"")

/** Used by DEBUG_PRINT as a microtick threshold */
$ty:time_t limit = $exp:limit;

/**
 * Circumvent optimizations that take advantage of C's undefined signed
 * integer wraparound behavior. FIXME: remove this hack, which is probably not
 * robust anyway if C is aggressive about inlining.
 */
static int _add(int a, int b) {
  return a + b;
}
|]
  where limit = maybe [cexp|ULONG_MAX|] (\i -> [cexp|$int:i|]) tickLimit

-- | Generate C definition for the main program and the top_return function,
-- both to be placed at the bottom of the generated C (or at least after the
-- type definitions and function prototypes).
genMain :: Program -> [C.Definition]
genMain program =
  [ [cedecl| void $id:top_return($ty:act_t *act) { return; } |]
  , [cedecl|
      int main(void) {
        $ty:act_t top = { .step = $id:top_return };

        /* Initialize variables to be passed to the main SSM procedure */
        $items:argInits

        /* Enter main SSM procedure */
        $id:fork($id:enter($args:enterArgs));

        $ty:time_t next;
        do {
          tick();
          next = next_event_time();
          set_now(next);
        } while (next != NO_EVENT_SCHEDULED);

        /* Print the final values of the arguments declared earlier */
        $items:refPrints
      }
    |]
  ]
 where
  enter = enter_ $ entry program
  enterArgs =
    [[cexp|&top|], [cexp|PRIORITY_AT_ROOT|], [cexp|DEPTH_AT_ROOT|]]
      ++ map enterArg (args program)
  enterArg (Left  ssmExp  ) = genExp [] ssmExp
  -- ^ TODO: this is buggy if ssmExp contains a var?? Maybe double check what's
  -- going on with that.
  enterArg (Right (ref, _)) = [cexp|&$id:ref|]

  argInits = concatMap argInit $ rights $ args program
  argInit (ref, typ) =
    [ [citem|$ty:(svt_ typ) $id:ref;|]
    , [citem|$id:(initialize_ typ)(&$id:ref);|]
    , [citem|$id:ref.value = 0;|]
      -- Args to the main SSM procedure are always given default values of 0.
    , [citem|DEBUG_SV_SET_VAR_NAME($id:ref.sv.debug, $string:ref);|]
    ]

  refPrints = map refPrint $ rights $ args program
  refPrint (ref, typ) = [citem|
    DEBUG_PRINT("result %s %s %s\n",  DEBUG_SV_GET_VAR_NAME($id:ref.sv.debug),
                                      DEBUG_SV_GET_TYPE_NAME($id:ref.sv.debug),
                                      DEBUG_SV_GET_VALUE_REPR($id:ref.sv.debug,
                                      &$id:ref.sv));|]

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

    } $id:(act_ $ name p);
  |]
 where
  param (n, Ref t) = [csdecl|$ty:(svt_ t) *$id:n;|]
  param (n, t    ) = [csdecl|$ty:(svt_ t) $id:n;|]

  local (n, t) = [csdecl|$ty:(svt_ t) $id:n;|]

  trig i = [csdecl|$ty:trigger_t $id:t;|] where t = "trig" ++ show i

-- | Generate the enter function for an SSM 'Procedure', where its struct is
-- allocated and initialized (partially; local variables' values are left
-- uninitialized).
genEnter :: TR (C.Definition, C.Definition)
genEnter = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  let act   = [cty|typename $id:act'|]
      act'  = act_ $ name p -- Hack to use this typename as expr in macros
      enter = enter_ $ name p
      step  = step_ $ name p
      params =
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
  param (n, Ref t) = [cparam|$ty:(svt_ t) *$id:n|]
  param (n, t    ) = [cparam|typename $id:(typeId t) $id:n|]

  initParam (n, Ref t) = [[cstm|acts->$id:n = $id:n;|]]
  initParam (n, t) =
    [ [cstm|$id:(initialize_ t)(&acts->$id:n);|]
    , [cstm|acts->$id:n.value = $id:n;|]
    ]

  initLocal (n, t) =
    [ [cstm| $id:(initialize_ t)(&acts->$id:n);|]
    , [cstm| DEBUG_SV_SET_VAR_NAME(acts->$id:n.sv.debug, $string:n); |]
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
  let act  = [cty|typename $id:act'|]
      act' = act_ $ name p -- Hack to use this typename as expr in macros
      step = step_ $ name p
  return
    ( [cedecl|void $id:step($ty:act_t *actg);|]
    , [cedecl|
        void $id:step($ty:act_t *actg) {
          $ty:act *acts = container_of(actg, $id:act', act);
          switch (actg->pc) {
          case 0:;
            $stms:cases

          case $int:final:;
            $stms:(map dequeue refs)
            $id:act_leave(actg, sizeof($ty:act));
            return;
          }
          printf("Error: Unreachable\n");
          assert(0);
        }
      |]
    )
  where dequeue (var, _) = [cstm|$id:unsched_event(&acts->$id:var.sv);|]

-- | Generate the list of statements from each 'Stm' in an SSM 'Procedure'.
--
-- Note that this compilation scheme might not work if the language were to
-- support return statements. This could be fixed by generating a break, and
-- moving the leave call to outside of the switch statement in 'genStep'.
genCase :: Stm -> TR [C.Stm]
genCase (NewRef n t v) = do
  locs <- map fst <$> gets locals
  let lvar = getVarName n
      lhs  = [cexp|&acts->$id:lvar|]
      rhs  = genExp locs v
  addLocal lvar t
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (GetRef n t (rvar, _)) = do
  locs <- map fst <$> gets locals
  let lvar = getVarName n
      lhs  = [cexp|&acts->$id:lvar|]
      rhs  = if rvar `elem` locs
        then [cexp|acts->$id:rvar.value|]
        else [cexp|acts->$id:rvar->value|]
  addLocal lvar t
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (SetRef (lvar, t) e) = do
  locs <- map fst <$> gets locals
  let lhs = if lvar `elem` locs
        then [cexp|&acts->$id:lvar|]
        else [cexp|acts->$id:lvar|]
      rhs = genExp locs e
  return [[cstm|$id:(assign_ t)($exp:lhs, actg->priority, $exp:rhs);|]]
genCase (SetLocal n t e) = do
  locs <- map fst <$> gets locals
  let lvar = getVarName n
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
        then [cexp|&acts->$id:lvar|]
        else [cexp|acts->$id:lvar|]
      rhs = genExp locs v
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
    then [cexp|&acts->$id:trig.sv|]
    else [cexp|&acts->$id:trig->sv|]
genCase (Fork cs) = do
  locs    <- map fst <$> gets locals
  caseNum <- nextCase
  let
    genCall i (r, as) = [cstm|$id:fork($id:(enter_ r)($args:enterArgs));|]
     where
      enterArgs =
        [ [cexp|actg|]
          , [cexp|actg->priority + $int:i * (1 << $exp:newDepth)|]
          , newDepth
          ]
          ++ map genArg as
      genArg (Left e) = genExp locs e
      genArg (Right (r, _)) =
        if r `elem` locs then [cexp|&acts->$id:r|] else [cexp|acts->$id:r|]

      newDepth = [cexp|actg->depth - $int:depthSub|]
      depthSub =
        (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Int

    genDebug (r, _) = r
  return
    $ [cstm| DEBUG_PRINT($string:((++ "\n") $ unwords $ "fork" : map fst cs)); |]
    : zipWith genCall [0 :: Int ..] cs
    ++ [ [cstm| actg->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
genCase Skip = return []

-- | Generate C expression from 'SSMExp' and a list of local variables.
genExp :: [String] -> SSMExp -> C.Exp
genExp _  (Var _ n              ) = [cexp|acts->$id:n.value|]
genExp _  (Lit _ (LInt32  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LUInt8  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LInt64  i    )) = [cexp|(typename i64) $int:i|]
genExp _  (Lit _ (LUInt64 i    )) = [cexp|(typename u64) $int:i|]
genExp _  (Lit _ (LBool   True )) = [cexp|true|]
genExp _  (Lit _ (LBool   False)) = [cexp|false|]
genExp ls (UOpE _ e Neg         ) = [cexp|- $exp:(genExp ls e)|]
genExp ls (UOpR _ (n, _) Changed)
  | n `elem` ls = [cexp|event_on(&acts->$id:n.sv)|]
  | otherwise   = [cexp|event_on(&acts->$id:n->sv)|]
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
  gen OLT    = [cexp|$exp:c1 < $exp:c2|]
  gen OEQ    = [cexp|$exp:c1 == $exp:c2|]
