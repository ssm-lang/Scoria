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

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | This function takes a `Program` and returns a string, which contains the content
-- of the generated C file.
compile_ :: Bool -> Maybe Int -> Program -> String
compile_ wantMain tickLimit program = pretty 120 $ pprList compUnit
 where
  preamble       = mkPreamble tickLimit

  (decls, defns) = concat2 $ unzip $ map mkProcedure $ Map.elems (funs program)

  compUnit       = preamble ++ decls ++ defns ++ [ mkMain program | wantMain ]

  concat2 (x, y) = (concat x, concat y)

-- | State maintained while compiling a procedure
data TRState = TRState
  { -- | Procedure we are compiling
    procedure :: Procedure
      -- | Which number has the next case?
  , ncase     :: Int
      -- | The size of the widest wait
  , numwaits  :: Int
      -- | Local references declared with var
  , locals    :: [(String, Type)]
  }

-- | Translation monad.
type TR a = State TRState a

-- | Run a TR computation.
runTR :: Procedure -> TR a -> a
runTR p tra = evalState tra $ TRState p 0 0 []

nextCase :: TR Int
nextCase = do
  n <- gets ncase
  modify $ \st -> st { ncase = n + 1 }
  return n

addLocal :: String -> Type -> TR ()
addLocal n t = modify $ \st -> st { locals = (n, t) : locals st }

maxWaits :: Int -> TR ()
maxWaits rs = modify $ \st -> st { numwaits = rs `max` numwaits st }

type CIdent = String

-- | Maps SSM Type to identifier of base type. Note that this unwraps reference
-- types and returns the base type.
typeId :: Type -> CIdent
typeId TInt32  = "int32"
typeId TInt64  = "int64"
typeId TUInt64 = "uint64"
typeId TUInt8  = "uint8"
typeId TBool   = "bool"
typeId (Ref t) = typeId t

topReturnId :: CIdent
topReturnId = "top_return"

fork :: CIdent
fork = "fork_routine"

act_enter :: CIdent
act_enter = "act_enter"

event_on :: CIdent
event_on = "event_on"

sensitize :: CIdent
sensitize = "sensitize"

desensitize :: CIdent
desensitize = "desensitize"

dequeue_event :: CIdent
dequeue_event = "dequeue_event"

stepId :: String -> CIdent
stepId routine = "step_" ++ routine

enter_ :: String -> CIdent
enter_ routine = "enter_" ++ routine

svtId :: String -> CIdent
svtId ty = "sv_" ++ ty ++ "_t"

actId :: String -> CIdent
actId ty = "act_" ++ ty ++ "_t"

trig_ :: Int -> CIdent
trig_ i = "trig" ++ show i

initialize_ :: Type -> CIdent
initialize_ ty = "initialize_" ++ typeId ty

assign_ :: Type -> CIdent
assign_ ty = "assign_" ++ typeId ty

later_ :: Type -> CIdent
later_ ty = "later_" ++ typeId ty

update_ :: Type -> CIdent
update_ ty = "update_" ++ typeId ty

-- | The type of the activation record base class
act_t :: C.Type
act_t = [cty|typename act_t|]

sv_t :: C.Type
sv_t = [cty|typename sv_t|]

time_t :: C.Type
time_t = [cty|typename peng_time_t|]

trigger_t :: C.Type
trigger_t = [cty|typename trigger_t|]

priority_t :: C.Type
priority_t = [cty|typename priority_t|]

depth_t :: C.Type
depth_t = [cty|typename depth_t|]

stepf_t :: C.Type
stepf_t = [cty|typename stepf_t|]

uint16_t :: C.Type
uint16_t = [cty|typename uint16_t|]

bool_t :: C.Type
bool_t = [cty|typename bool|]

svt_ :: Type -> C.Type
svt_ ty = [cty|typename $id:(svtId $ typeId ty)|]

-- | FIXME: This only works because we don't have nested Refs (yet)
param_ :: Type -> C.Type
param_ (Ref ty) = [cty|$ty:(svt_ ty) *|]
param_ ty       = [cty|typename $id:(typeId ty)|]

act_ :: String -> C.Type
act_ ty = [cty|typename $id:(actId ty)|]

mkPreamble :: Maybe Int -> [C.Definition]
mkPreamble tickLimit = [cunit|
$esc:("#include \"peng-platform.h\"")
$esc:("#include \"peng.h\"")
$esc:("#include <stdio.h>")
$esc:("#include <stdint.h>")
/* Are these comments preserved? */

extern void $id:dequeue_event($ty:sv_t *var);

extern $ty:time_t now;
extern int can_schedule($ty:sv_t *var);

$ty:time_t limit = $exp:limit;

static int __add(int a, int b) {
  return a + b;
}
|]
 where
  limit = maybe [cexp|ULONG_MAX|] promoteExp tickLimit

  promoteExp i = [cexp|$int:i|]

topReturn :: C.Definition
topReturn = [cedecl| void $id:topReturnId($ty:act_t *act) { return; } |]

-- | Generate C definition for main program
-- TODO items: (1) lift per-type default value and formatters from here
--             (2) remove pointer cast
--             (3) centralize debug logic
mkMain :: Program -> C.Definition
mkMain program = [cedecl|
    void main(void) {
      $ty:act_t top = { .step = $id:topReturnId };

      $items:refInits

      $id:fork(($ty:act_t *) /* FIXME */ $id:enter($args:enterArgs));

      tick();

      DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);

      for (;;) {
        now = next_event_time();
        if(now == NO_EVENT_SCHEDULED)
          break;
        tick();
        DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
      }

      $items:refPrints
    }
  |]
 where
  enter = enter_ $ main program

  enterArgs =
    [[cexp|&top|], [cexp|PRIORITY_AT_ROOT|], [cexp|DEPTH_AT_ROOT|]]
      ++ map enterArg (args program)

  enterArg (Left  ssmExp  ) = mkExp [] ssmExp -- FIXME: this is buggy if ssmExp contains a var
  enterArg (Right (ref, _)) = [cexp|&$id:ref|]

  refInits  = concatMap refInit $ rights $ args program

  refPrints = map refPrint $ rights $ args program

  refInit (ref, typ) =
    [ [citem|$ty:(svt_ typ) $id:ref;|]
    , [citem|$id:(initialize_ typ)(&$id:ref);|]
    , [citem|$id:ref.value = /* FIXME */ 0;|]
    ]

  refPrint (ref, _) =
    [citem|printf($string:fmtString, /* FIXME */ (long) $id:ref.value);|] {-typ-}
    where fmtString = "result " ++ ref ++ " %ld\n"

mkProcedure :: Procedure -> ([C.Definition], [C.Definition])
mkProcedure p = runTR p $ do
  (stepDecl , stepDefn ) <- mkStep
  (enterDecl, enterDefn) <- mkEnter
  structDefn             <- mkStruct
  return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])

mkStruct :: TR C.Definition
mkStruct = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  return [cedecl|
    typedef struct {
      $sdecls:aCTIVATION_RECORD_FIELDS

      $sdecls:(map param (arguments p))
      $sdecls:(map local ls)
      $sdecls:(map trig [1..ts])

    } $id:(actId $ name p);
  |]
 where
  aCTIVATION_RECORD_FIELDS =
    [ [csdecl|$ty:stepf_t *step;|]
    , [csdecl|struct act *caller;|]
    , [csdecl|$ty:uint16_t pc;|]
    , [csdecl|$ty:uint16_t children;|]
    , [csdecl|$ty:priority_t priority;|]
    , [csdecl|$ty:depth_t depth;|]
    , [csdecl|$ty:bool_t scheduled;|]
    ]

  param (n, t) = [csdecl|$ty:(param_ t) $id:n;|]

  local (n, t) = [csdecl|$ty:(svt_ t) $id:n;|]

  trig i = [csdecl|$ty:trigger_t $id:t;|] where t = "trig" ++ show i

mkEnter :: TR (C.Definition, C.Definition)
mkEnter = do
  p  <- gets procedure
  ts <- gets numwaits
  ls <- gets locals
  let act   = act_ (name p)
      enter = enter_ (name p)
      step  = stepId (name p)
      params =
        [cparams|$ty:act *caller, $ty:priority_t priority, $ty:depth_t depth|]
          ++ map param (arguments p)
  return
    ( [cedecl|$ty:act *$id:enter($params:params);|]
    , [cedecl|
        $ty:act *$id:enter($params:params) {
          $ty:act_t *gen_act = $id:act_enter(sizeof($ty:act), $id:step, caller, priority, depth);
          $ty:act *act = ($ty:act *) gen_act;

          $stms:(map initParam (arguments p))
          $stms:(map initLocal ls)
          $stms:(map initTrig [1..ts])

          return act;
        }
      |]
    )
 where
  param (n, t) = [cparam|$ty:(param_ t) $id:n|]

  -- initParam (n, Ref t) = -- TODO: do we need to treat refs differently?
  initParam (n, _) = [cstm|act->$id:n = $id:n;|]

  initLocal (n, t) = [cstm| $id:(initialize_ t)(&act->$id:n);|]

  initTrig i = [cstm| act->$id:trig.act = gen_act;|]
    where trig = "trig" ++ show i

mkStep :: TR (C.Definition, C.Definition)
mkStep = do
  p     <- gets procedure
  _     <- nextCase -- Toss away 0th case
  cases <- concat <$> mapM mkCase (body p)
  refs  <- gets locals
  final <- nextCase
  let step = stepId (name p)
      act  = act_ (name p)
  return
    ( [cedecl|void $id:step($ty:act_t *gen_act);|]
    , [cedecl|
        void $id:step($ty:act_t *gen_act) {
          $ty:act *act = ($ty:act *) gen_act; /* FIXME: remove cast */
          switch (gen_act->pc) {
          case 0:;
            $stms:cases

          case $int:final:; /* FIXME: this last case is not needed */
            $stms:(map dequeue refs)
            leave(gen_act, sizeof($ty:act));
            return;
          }
          printf("Error: Unreachable\n");
          assert(0);
        }
      |]
    )
  where dequeue (s, _) = [cstm|$id:dequeue_event(($ty:sv_t *) &act->$id:s);|]

-- | TODO: doc
--
-- Note that this compilation scheme might not work if the language were to
-- support return statements. This could be fixed by placing synthesizing
-- a break, and moving the leave call to outside of the switch statement.
--
-- TODOs: remove hard-coded `act` stuff
mkCase :: Stm -> TR [C.Stm]
mkCase (NewRef n t v) = do
  params <- map fst . arguments <$> gets procedure
  let lvar = getVarName n
      lhs  = [cexp|&act->$id:lvar|]
      rhs  = mkExp params v
  addLocal lvar t
  return [[cstm|$id:(assign_ t)($exp:lhs, gen_act->priority, $exp:rhs);|]]
mkCase (GetRef n t (rvar, _)) = do
  refs <- map fst <$> gets locals
  let lvar = getVarName n
      lhs  = [cexp|&act->$id:lvar|]
      rhs  = if rvar `elem` refs
        then [cexp|act->$id:rvar.value|]
        else [cexp|act->$id:rvar->value|]
  return [[cstm|$id:(assign_ t)($exp:lhs, gen_act->priority, $exp:rhs);|]]
mkCase (SetRef (lvar, t) e) = do
  params <- map fst . arguments <$> gets procedure
  refs   <- map fst <$> gets locals
  let lhs = if lvar `elem` refs
        then [cexp|&act->$id:lvar|]
        else [cexp|act->$id:lvar|]
      rhs = mkExp params e
  return [[cstm|$id:(assign_ t)($exp:lhs, gen_act->priority, $exp:rhs);|]]
mkCase (SetLocal n t e) = do
  params <- map fst . arguments <$> gets procedure
  let lvar = getVarName n
      lhs  = [cexp|&act->$id:lvar|]
      rhs  = mkExp params e
  return [[cstm|$id:(assign_ t)($exp:lhs, gen_act->priority, $exp:rhs);|]]
mkCase (If c t e) = do
  params <- map fst . arguments <$> gets procedure
  let cnd = mkExp params c
  thn <- concat <$> mapM mkCase t
  els <- concat <$> mapM mkCase e
  return [[cstm| if ($exp:cnd) { $stms:thn } else { $stms:els }|]]
mkCase (While c b) = do
  params <- map fst . arguments <$> gets procedure
  let cnd = mkExp params c
  bod <- concat <$> mapM mkCase b
  return [[cstm| while ($exp:cnd) { $stms:bod } |]]
mkCase (After d (lvar, t) v) = do
  params <- map fst . arguments <$> gets procedure
  refs   <- map fst <$> gets locals
  let del = mkExp params d
      lhs = if lvar `elem` refs
        then [cexp|&act->$id:lvar|]
        else [cexp|act->$id:lvar|]
      rhs = mkExp params v
  return [[cstm| $id:(later_ t)($exp:lhs, $exp:del, $exp:rhs);|]]
mkCase (Changed n t (rvar, _)) = do
  refs <- map fst <$> gets locals
  let
    lvar = getVarName n
    lhs  = [cexp|&act->$id:lvar|]
    rhs =
      if rvar `elem` refs then [cexp|&act->$id:rvar|] else [cexp|act->$id:rvar|]
  return
    [ [cstm| $id:(assign_ t)($exp:lhs, act->priority, $id:event_on(($ty:sv_t *) $exp:rhs));|]
    ]
mkCase (Wait ts) = do
  caseNum <- nextCase
  maxWaits $ length ts
  refs <- map fst <$> gets locals
  let trigs = zip [1 ..] $ map (mkTrig refs) ts
  return
    $  fmap sensitizeTrig trigs
    ++ [ [cstm| gen_act->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
    ++ fmap desensitizeTrig trigs
 where
  sensitizeTrig (i, trig) =
    [cstm|$id:sensitize(($ty:sv_t *) $exp:trig, &act->$id:(trig_ i));|]

  desensitizeTrig (i, _) = [cstm|$id:desensitize(&act->$id:(trig_ i));|]

  mkTrig refs (trig, _) =
    if trig `elem` refs then [cexp|&act->$id:trig|] else [cexp|act->$id:trig|]
mkCase (Fork cs) = do
  params  <- map fst . arguments <$> gets procedure
  refs    <- map fst <$> gets locals
  caseNum <- nextCase
  let depthSub =
        (ceiling $ logBase (2 :: Double) $ fromIntegral $ length cs) :: Integer
      newDepth = [cexp|act->depth - $int:depthSub|]
      mkArgs i extraArgs =
        [ [cexp|act|]
          , [cexp|act->priority + $int:i * (1 << $exp:newDepth)|]
          , newDepth
          ]
          ++ extraArgs
      mkCall i (r, as) =
        [cstm|$id:fork(($ty:act_t *) $id:(enter_ r)($args:enterArgs));|]
        where enterArgs = mkArgs i $ map mkArg as
      mkArg (Left e) = mkExp params e
      mkArg (Right (r, _)) =
        if r `elem` refs then [cexp|&act->$id:r|] else [cexp|act->$id:r|]
  return
    $  zipWith mkCall [1 :: Int ..] cs
    ++ [ [cstm| gen_act->pc = $int:caseNum; |]
       , [cstm| return; |]
       , [cstm| case $int:caseNum: ; |]
       ]
mkCase Skip = return []

-- TODO: double check that the quasi quote thing handles parenthesization and
-- precedence for us.
mkExp :: [String] -> SSMExp -> C.Exp
mkExp _ (Lit _ (LInt32  i    )) = [cexp|$int:i|]
mkExp _ (Lit _ (LUInt8  i    )) = [cexp|$int:i|]
mkExp _ (Lit _ (LInt64  i    )) = [cexp|$int:i|]
mkExp _ (Lit _ (LUInt64 i    )) = [cexp|$int:i|]
mkExp _ (Lit _ (LBool   True )) = [cexp|true|]
mkExp _ (Lit _ (LBool   False)) = [cexp|false|]
mkExp params (Var _ n) =
  if n `elem` params then [cexp|act->$id:n|] else [cexp|act->$id:n.value|]
mkExp params (UOp _ e Neg) = [cexp|- $exp:(mkExp params e)|]
mkExp params (BOp TInt32 e1 e2 OPlus) =
  [cexp|__add($exp:(mkExp params e1), $exp:(mkExp params e2))|]
mkExp params (BOp _ e1 e2 op) = c op
 where
  (c1, c2) = (mkExp params e1, mkExp params e2)

  c OPlus  = [cexp|$exp:c1 + $exp:c2|]
  c OMinus = [cexp|$exp:c1 - $exp:c2|]
  c OTimes = [cexp|$exp:c1 * $exp:c2|]
  c OLT    = [cexp|$exp:c1 < $exp:c2|]
  c OEQ    = [cexp|$exp:c1 == $exp:c2|]
