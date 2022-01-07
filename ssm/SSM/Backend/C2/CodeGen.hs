{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C2.CodeGen where

import SSM.Core hiding (Program(..), Procedure(..), Stm(..))
import SSM.Core.Backend

import SSM.Backend.C2.Identifiers
import SSM.Backend.C2.IR hiding (localrefs)

import Data.List
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

compile :: Program C2 -> String
compile p = undefined

compilationUnit :: Program C2 -> [C.Definition]
compilationUnit p = undefined

initializeProgram :: Program C2 -> C.Definition
initializeProgram p = undefined

data ProcedureInfo = ProcedureInfo
  { triggerIDs   :: [Int]
  , sensitizes   :: Map.Map Int Int
  , desensitizes :: Map.Map Int Int
  , localrefs    :: [Reference]
  }

data TriggerState = TriggerState
  { availableTriggers :: [Int]
  , widestWait        :: Int
  , currentSensitized :: Map.Map Reference Int
  , sensitizes_       :: Map.Map Int Int
  , desensitizes_     :: Map.Map Int Int
  , localrefs_        :: [Reference]
  }

genProcedureInfo :: Procedure -> ProcedureInfo
genProcedureInfo p =
    let st  = TriggerState [0..] 0 Map.empty Map.empty Map.empty []
        st' = execState (gatherInfo $ body p) st
        lr  = nub $ Map.elems (sensitizes_ st') <> Map.elems (desensitizes_ st')
    in ProcedureInfo lr (sensitizes_ st') (desensitizes_ st') (localrefs_ st')
  where
      gatherInfo :: [Stm] -> State TriggerState ()
      gatherInfo stms = forM_ stms $ \stm -> case stm of
          CreateRef n id t ->
            modify $ \st -> st { localrefs_ = makeDynamicRef id t : localrefs_ st }
          Sensitize n r    -> sensitize n r
          Desensitize n r  -> desensitize n r
          If n c thn els   -> gatherInfo thn >> gatherInfo els
          While n c bdy    -> gatherInfo bdy
          _                -> return ()

      sensitize :: Int -> Reference -> State TriggerState ()
      sensitize n r = do
          st <- get

          let (t:triggers) = availableTriggers st
              s            = Map.insert n t $ sensitizes_ st
              ww           = if widestWait st < t then t else widestWait st
              cs           = Map.insert r t $ currentSensitized st

          put $ st { availableTriggers = triggers
                   , sensitizes_       = s
                   , widestWait        = ww
                   , currentSensitized = cs
                   }

      desensitize :: Int -> Reference -> State TriggerState ()
      desensitize n r = do
          st <- get

          let t  = case Map.lookup r (currentSensitized st) of
                     Just trigger -> trigger
                     Nothing      -> error "error in desensitize"
              cs = Map.delete r $ currentSensitized st
              d  = Map.insert n t (desensitizes_ st)
              

          put $ st { availableTriggers = t : availableTriggers st
                   , desensitizes_     = d
                   , currentSensitized = cs
                   }

-- compile procedures

data CompiledProcedure = CompiledProcedure
  { struct     :: C.Definition
  , prototypes :: [C.Definition]
  , methods    :: [C.Definition]
  }

compileProcedure :: Procedure -> ProcedureInfo -> CompiledProcedure
compileProcedure p procedureInfo =
    CompiledProcedure
      genStruct
      
      [ genEnterPrototype
      , genStepPrototype
      ]
      
      [ genEnter
      , genStep
      ]
  where
      step_proc :: String
      step_proc = step_ $ identName $ name p

      ssm_enter_proc :: String
      ssm_enter_proc = ssm_enter_ $ identName $ name p

      act_proc :: String
      act_proc = act_ $ identName $ name p

      act_proc_t :: C.Type
      act_proc_t = asType act_proc

      genStruct :: C.Definition
      genStruct =
          let triggers  = triggerIDs procedureInfo
              triggers' = flip map triggers $ \i ->
                [csdecl| $ty:struct_ssm_trigger $id:("trigger" <> show i); |]

              lr        = localrefs procedureInfo
              lrefs     = flip map lr $ \r ->
                [csdecl| $ty:ssm_value_t $id:(refName r); |]

              args'     = flip map (arguments p) $ \(id,t) ->
                [csdecl| $ty:ssm_value_t $id:(identName id); |]

          in [cedecl|

            typedef struct {
                $ty:ssm_act_t act;

                // locally declared references
                $sdecls:lrefs

                // procedure arguments
                $sdecls:args'

                // triggers
                $sdecls:triggers'
            } $id:act_proc;

          |]

      genEnter :: C.Definition
      genEnter =
          [cedecl|

            $ty:ssm_act_t $id:ssm_enter_proc($params:(staticArgs <> args)) {
              $ty:act_proc_t *cont = container_of
                  ( $id:ssm_enter
                        ( sizeof(*$id:cont)
                        , $id:step_proc
                        , parent
                        , priority
                        , depth
                        )
                  , $id:act_proc
                  , act
                  );
              $items:assigns
              return &cont->act;
            }

            |]
      
      args    :: [C.Param]
      assigns :: [C.BlockItem]
      (args, assigns) = unzip $ flip map (arguments p) $ \(id,_) ->
          ( [cparam| $ty:ssm_value_t $id:(identName id) |]
          , [citem| $id:cont->$id:(identName id) = $id:(identName id); |])

      staticArgs :: [C.Param]
      staticArgs = [ [cparam| $ty:struct_ssm_act *parent  |]
                   , [cparam| $ty:ssm_priority_t priority |]
                   , [cparam| $ty:ssm_depth_t depth       |]
                   ]

      genEnterPrototype :: C.Definition
      genEnterPrototype =
          [cedecl| $ty:ssm_act_t $id:ssm_enter_proc($params:(staticArgs <> args)); |]

      genStep :: C.Definition
      genStep =
          let stms = concatMap genStm $ body p
          in [cedecl|

            void $id:step_proc ($ty:struct_ssm_act *act) {
                $ty:act_proc_t *cont = container_of(act, $id:act_proc, act);
                switch(act->pc) {
                    $items:stms
                }
            }

            |]
        where
            genStm :: Stm -> [C.BlockItem]
            genStm (CreateRef n id t) = undefined
            genStm (SetRef n r e) =
                let lrefs = localrefs procedureInfo
                    e' = compileExp lrefs e
                in [citems| $id:ssm_assign(&$id:(accessRef r), $id:cont->prio, $exp:e'); |]
            genStm (If n c thn els) =
                let lrefs = localrefs procedureInfo
                    c'   = compileExp lrefs c
                    thn' = concatMap genStm thn
                    els' = concatMap genStm els
                in [citems| if($exp:c') { $items:thn' } else { $items:els' } |]
            genStm (While n c bdy) =
                let lrefs = localrefs procedureInfo
                    c'    = compileExp lrefs c
                    bdy'  = concatMap genStm bdy
                in [citems| while($exp:c') { $items:bdy' } |]
            genStm (Skip n) = []
            genStm (After n d r v) =
                let lrefs = localrefs procedureInfo
                    d' = compileExp lrefs d
                    v' = compileExp lrefs v
                    r' = accessRef r
                in [citems| $id:ssm_later(&$id:r', $id:ssm_now() + $exp:d', $exp:v'); |]
            genStm (Sensitize n r) =
                let t       = fetchTrigger n
                    trigger = "trigger" <> show t
                in [citems| $id:ssm_sensitize(&$id:(accessRef r), &$id:cont->$id:trigger); |]
            genStm (Desensitize n r) =
                let t = fetchTrigger n
                    trigger = "trigger" <> show t
                in [citems| $id:ssm_desensitize(&$id:(accessRef r), &$id:cont->$id:trigger); |]
            genStm (Fork n id k i args) = [citems| $id:ssm_fork(); |]
            genStm (Yield n)       = [citems| $id:cont->pc += 1; return; |]
            genStm (Terminate n)   = [citems| ssm_leave(&cont->act, sizeof($id:act_proc)); |]
            genStm (Dup n r)       = [citems| $id:ssm_dup(&$id:(accessRef r)); |]
            genStm (Drop n r)      = [citems| $id:ssm_drop(&$id:(accessRef r)); |]
            genStm (SetState n st) = [citems| case $int:st:; |]

            fetchTrigger :: Int -> Int
            fetchTrigger n = do
                case Map.lookup n (sensitizes procedureInfo) of
                    Just i -> i
                    Nothing -> case Map.lookup n (desensitizes procedureInfo) of
                        Just i -> i
                        Nothing -> error "error in trigger allocation"

      genStepPrototype :: C.Definition
      genStepPrototype =
          [cedecl| void $id:step_proc($ty:struct_ssm_act *act); |]

-- compile expressions

compileExp :: [Reference] -> SSMExp -> C.Exp
compileExp localrefs (Var t id)       = undefined
compileExp localrefs (Lit t l)        = compileLit localrefs l
compileExp localrefs (UOpE t e op)    = compileUOpE localrefs e op
compileExp localrefs (UOpR t r op)    = compileUOpR localrefs r op
compileExp localrefs (BOp t e1 e2 op) = compileBinOp localrefs e1 e2 op

compileLit :: [Reference] -> SSMLit -> C.Exp
compileLit localrefs (LUInt8 w)  = [cexp| $id:marshal($uint:w) |]
compileLit localrefs (LUInt32 w) = [cexp| $id:marshal($uint:w) |]
compileLit localrefs (LUInt64 w) = [cexp| $id:marshal($uint:w) |]
compileLit localrefs (LInt32 i)  = [cexp| $id:marshal($int:i) |]
compileLit localrefs (LInt64 i)  = [cexp| $id:marshal($int:i) |]
compileLit localrefs LEvent      = [cexp| $id:marshal(0) |]
compileLit localrefs (LBool b)   = let lit = if b then [cexp| true|] else [cexp|false|]
                                   in [cexp| $id:marshal($exp:lit) |]

compileUOpE :: [Reference] -> SSMExp -> UnaryOpE -> C.Exp
compileUOpE localrefs e Neg = [cexp| $id:marshal(- $id:unmarshal($exp:(compileExp localrefs e))) |]
compileUOpE localrefs e Not = [cexp| $id:marshal(! $id:unmarshal($exp:(compileExp localrefs e))) |]

compileUOpR :: [Reference] -> Reference -> UnaryOpR -> C.Exp
compileUOpR localrefs r op = case op of
    Changed -> [cexp| $id:(accessRef r)->last_updated == $id:ssm_now() |]
    Deref   -> [cexp| $id:ssm_deref($id:(accessRef r)) |]

compileBinOp :: [Reference] -> SSMExp -> SSMExp -> BinOp -> C.Exp
compileBinOp localrefs e1 e2 op = case op of
    OPlus   -> [cexp| $id:marshal($exp:e1' + $exp:e2') |]
    OMinus  -> [cexp| $id:marshal($exp:e1' - $exp:e2') |]
    OTimes  -> [cexp| $id:marshal($exp:e1' * $exp:e2') |]
    ODiv    -> [cexp| $id:marshal($exp:e1' / $exp:e2') |]
    ORem    -> [cexp| $id:marshal($exp:e1' % $exp:e2') |]
    OMin    -> [cexp| $id:marshal($exp:e1' < $exp:e2' ? $exp:e1' : $exp:e2') |]
    OMax    -> [cexp| $id:marshal($exp:e1' < $exp:e2' ? $exp:e2' : $exp:e1') |]
    OLT     -> [cexp| $id:marshal($exp:e1' < $exp:e2') |]
    OEQ     -> [cexp| $id:marshal($exp:e1' == $exp:e2') |]
    OAnd    -> [cexp| $id:marshal($exp:e1' && $exp:e2') |]
    OOr     -> [cexp| $id:marshal($exp:e1' || $exp:e2') |]
    OLShift -> [cexp| $id:marshal($exp:e1' << $exp:e2') |]
    ORShift -> [cexp| $id:marshal($exp:e1' >> $exp:e2') |]
    OBAnd   -> [cexp| $id:marshal($exp:e1' & $exp:e2') |]
    OBOr    -> [cexp| $id:marshal($exp:e1' | $exp:e2') |]
    OBXor   -> [cexp| $id:marshal($exp:e1' ^ $exp:e2') |]
  where
      e1' = [cexp| $id:unmarshal($exp:(compileExp localrefs e1)) |]
      e2' = [cexp| $id:unmarshal($exp:(compileExp localrefs e2)) |]
