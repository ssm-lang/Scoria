{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module SSM.Backend.C2.CodeGen where

import SSM.Core hiding (Program(..), Procedure(..), Stm(..), SSMExp(..), QueueContent(..))
import SSM.Core.Backend

import SSM.Trace.Trace

import SSM.Backend.C2.Identifiers as ID
import SSM.Backend.C2.IR as IR hiding (localrefs)

import Data.Proxy
import Data.List
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C
import qualified Language.C                    as LC

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )


instance ToIdent Ident where
    toIdent = LC.Id . ident

compile :: Program C2 -> String
compile = pretty 120 . pprList . compilationUnit (CompilationFlags True)

data CompilationFlags = CompilationFlags
  { debug :: Bool
  }

compilationUnit :: CompilationFlags -> Program C2 -> [C.Definition]
compilationUnit cf p =
    includes                                        <>
    SSM.Backend.C2.CodeGen.globalDeclarations init' <>
    map struct procs                                <>
    concatMap prototypes procs                      <>
    concatMap methods procs                         <>
    [ SSM.Backend.C2.CodeGen.init init'
    , exit init'
    ]
  where
      procs :: [CompiledProcedure]
      procs = flip map (Map.elems $ funs p) $ \p ->
          (compileProcedure cf) p (genProcedureInfo p)

      init' :: ProgramInit
      init' = genProgramInit p

      includes :: [C.Definition]
      includes = [ [cedecl| $esc:("#include <ssm.h>") |]
                 , [cedecl| $esc:("#include <ssm-internal.h>") |]
                 ] <>
                 if debug cf
                     then [ [cedecl| $esc:("#include <ssm-test-platform.h>") |] ]
                     else []

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
            modify $ \st -> st { localrefs_ = makeDynamicRef id (mkReference t) : localrefs_ st }
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

-- compile program init

data ProgramInit = ProgramInit
  { globalDeclarations :: [C.Definition]
  , init               :: C.Definition
  , exit               :: C.Definition
  }

genProgramInit :: Program C2 -> ProgramInit
genProgramInit p =
  ProgramInit
    (globalDeclarations <> declareReferences)

    ([cedecl| void $id:ssm_program_init(void) {
                  $items:setupReferences
                  $items:staticInitialization
                  $items:dupReferences
                  $items:initialForks
              }
  
    |])

    ([cedecl| void $id:ssm_program_exit(void) {
                  $items:dropReferences
              }
    |])

  where
      globalReferences :: [Reference]
      globalReferences = concatMap (declaredReferences (Proxy @C2)) $ peripherals p

      declareReferences :: [C.Definition]
      declareReferences = flip map globalReferences $ \r ->
          [cedecl| $ty:ssm_value_t $id:(refName r); |]

      -- | FIXME the marshal(0) is not the best, and should be done with some typeclass
      setupReferences :: [C.BlockItem]
      setupReferences = flip concatMap globalReferences $ \r ->
          [citems| $id:(refName r) = $id:ssm_new($id:ssm_builtin, $id:ssm_sv_t);
                   $id:ssm_sv_init($id:(accessRef r), $id:marshal(0));
         |]

      dupReferences :: [C.BlockItem]
      dupReferences = flip map globalReferences $ \r ->
          [citem| $id:ssm_dup($id:(refName r)); |]

      dropReferences :: [C.BlockItem]
      dropReferences = flip map globalReferences $ \r ->
          [citem| $id:ssm_drop($id:(refName r)); |]

      initialForks :: [C.BlockItem]
      initialForks =
          let k = length (initialQueueContent p)

              f i (SSMProcedure id args)  =
                  let ds = [cexp| $int:(ceiling $ logBase (2 :: Double) $ fromIntegral $ k :: Int) |]
                      d  = [cexp| $id:ssm_root_depth - $exp:ds |]
                      p  = [cexp| $id:ssm_root_priority + ($int:i * (1 << $exp:ds)) |]
                      args' = flip map args $ \a -> case a of
                          Left e  -> compileExp [] e
                          Right r -> [cexp| $id:(refName r) |]
                  in [citems| $id:ssm_activate($id:(ssm_enter_ $ identName id)
                                                      ( &$id:ssm_top_parent
                                                      , $exp:p
                                                      , $exp:d
                                                      , $args:args'
                                                      )
                                              ); |]
              f i (OutputHandler handler) = gen_handler @C2 handler k i

          in concat $ zipWith f [0..] (initialQueueContent p)

      -- peripheral management

      globalDeclarations :: [C.Definition]
      globalDeclarations = concatMap (SSM.Core.globalDeclarations (Proxy @C2)) $ peripherals p

      staticInitialization :: [C.BlockItem]
      staticInitialization = concatMap (SSM.Core.staticInitialization (Proxy @C2)) $ peripherals p

-- compile procedures

data CompiledProcedure = CompiledProcedure
  { struct     :: C.Definition
  , prototypes :: [C.Definition]
  , methods    :: [C.Definition]
  }

compileProcedure :: CompilationFlags -> Procedure -> ProcedureInfo -> CompiledProcedure
compileProcedure cf p procedureInfo =
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

              args'     = flip map (arguments p) $ \(id,_) ->
                [csdecl| $ty:ssm_value_t $id:id; |]

          in [cedecl|

            typedef struct {
                // super struct
                $ty:ssm_act_t $id:act;
                
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

            $ty:ssm_act_t *$id:ssm_enter_proc($params:(staticArgs <> args)) {
              $ty:act_proc_t *cont = container_of
                  ( $id:ssm_enter
                        ( sizeof(*$id:cont)
                        , $id:step_proc
                        , parent
                        , priority
                        , depth
                        )
                  , $id:act_proc
                  , $id:act
                  );
              $items:inits
              $items:assigns
              return &$id:cont->$id:act;
            }

            |]
      inits :: [C.BlockItem]
      inits = flip map (localrefs procedureInfo) $ \ref ->
          [citem| $id:cont->$id:(refName ref) = $id:nil_pointer(); |]

      args    :: [C.Param]
      assigns :: [C.BlockItem]
      (args, assigns) = unzip $ flip map (arguments p) $ \(id,_) ->
          ( [cparam| $ty:ssm_value_t $id:id |]
          , [citem| $id:cont->$id:id = $id:id; |])

      staticArgs :: [C.Param]
      staticArgs = [ [cparam| $ty:struct_ssm_act *parent  |]
                   , [cparam| $ty:ssm_priority_t priority |]
                   , [cparam| $ty:ssm_depth_t depth       |]
                   ]

      genEnterPrototype :: C.Definition
      genEnterPrototype =
          [cedecl| $ty:ssm_act_t *$id:ssm_enter_proc($params:(staticArgs <> args)); |]

      genStep :: C.Definition
      genStep =
          let stms = concatMap genStm $ body p
          in [cedecl|

            void $id:step_proc ($ty:struct_ssm_act *$id:act) {
                $ty:act_proc_t *cont = container_of($id:act, $id:act_proc, $id:act);

                $items:debugStms

                switch($id:act->pc) {
                    $items:stms
                }
            }

            |]
        where
            debugStms :: [C.BlockItem]
            debugStms = if debug cf
                then [citems|
                         $id:debug_trace($string:(renderActStepBegin $ identName $ name p));
                         $items:(map (((uncurry . flip) renderSV)) $ sortOn fst $ arguments p)
                         $items:(map renderSVifChanged $ sortOn refName (localrefs $ procedureInfo))
                         $id:debug_microtick();                
                     |]
                else []

            genStm :: Stm -> [C.BlockItem]
            genStm (CreateRef n id t) = []
            genStm (InitializeRef n r e) =
                let lrefs = localrefs procedureInfo
                    e'    = compileExp lrefs e
                in [citems| $id:cont->$id:(refName r) = $id:ssm_new_sv($exp:e'); |]
            genStm (SetRef n r e) =
                let lrefs = localrefs procedureInfo
                    e' = compileExp lrefs e
                in [citems| $id:ssm_assign($id:ssm_to_sv($id:(accessRef r)), $id:act->$id:priority, $exp:e'); |]
            genStm (If n c thn els) =
                let lrefs = localrefs procedureInfo
                    c'   = compileExp lrefs $ IR.unmarshal c
                    thn' = concatMap genStm thn
                    els' = concatMap genStm els
                in [citems| if($exp:c') { $items:thn' } else { $items:els' } |]
            genStm (While n c bdy) =
                let lrefs = localrefs procedureInfo
                    c'    = compileExp lrefs $ IR.unmarshal c
                    bdy'  = concatMap genStm bdy
                in [citems| while($exp:c') { $items:bdy' } |]
            genStm (Skip n) = []
            genStm (After n d r v) =
                let lrefs = localrefs procedureInfo
                    d' = compileExp lrefs $ IR.unmarshal d
                    v' = compileExp lrefs v
                    r' = accessRef r
                in [citems| $id:ssm_later($id:ssm_to_sv($id:r'), $id:ssm_now() + $exp:d', $exp:v'); |]
            genStm (Sensitize n r) =
                let t       = fetchTrigger n
                    trigger = "trigger" <> show t

                    dt      = if debug cf
                        then [citems| $id:debug_trace($string:(show $ ActSensitize $ refName r)); |]
                        else []

                in [citems| $items:dt;
                            $id:ssm_sensitize($id:ssm_to_sv($id:(accessRef r)), &$id:cont->$id:trigger); |]
            genStm (Desensitize n r) =
                let t = fetchTrigger n
                    trigger = "trigger" <> show t
                in [citems| $id:ssm_desensitize(&$id:cont->$id:trigger); |]
            genStm (Fork n id k i args) = fork (Fork n id k i args)
            genStm (Yield n)       = [citems| $id:act->pc += 1; return; |]
            genStm (Terminate n)   = [citems| ssm_leave(&cont->$id:act, sizeof($id:act_proc)); |]
            genStm (Dup n v)       = [citems| $id:ssm_dup($exp:(genValue v)); |]
            genStm (Drop n v)      = [citems| $id:ssm_drop($exp:(genValue v)); |]
            genStm (SetState n st) = [citems| case $int:st:; |]
            genStm (Comment str)   = [citems| $esc:("//  " <> str); |]

            genValue :: Value -> C.Exp
            genValue (E e) = compileExp (localrefs procedureInfo) e
            genValue (R r) = [cexp| $id:(accessRef r) |]

            fork :: Stm -> [C.BlockItem]
            fork (Fork n id k i args) =
                let ds = [cexp| $int:(ceiling $ logBase (2 :: Double) $ fromIntegral $ k :: Int) |]
                    d  = [cexp| $id:act->depth - $exp:ds |]
                    p  = [cexp| $id:act->priority + ($int:i * (1 << $exp:ds)) |]
                    
                    lrefs = localrefs procedureInfo
                    
                    args' = flip map args $ \a -> case a of
                        Left e -> compileExp lrefs e
                        Right r -> [cexp| $id:(accessRef r) |]
                    
                    dt = if debug cf
                           then [citems| if($id:act->depth < $exp:ds) {
                                             $id:ssm_throw($id:ssm_exhausted_priority);
                                         }
                                         $id:debug_trace($string:(show $ ActActivate $ identName $ id));
                                |]
                           else []

                in [citems| $items:dt
                            $id:ssm_activate($id:(ssm_enter_ $ identName id)
                                              ( $id:act
                                              , $exp:p
                                              , $exp:d
                                              , $args:args'
                                              )
                                            ); |]
            fork _ = error "not a fork"

            fetchTrigger :: Int -> Int
            fetchTrigger n = do
                case Map.lookup n (sensitizes procedureInfo) of
                    Just i -> i
                    Nothing -> case Map.lookup n (desensitizes procedureInfo) of
                        Just i -> i
                        Nothing -> error "error in trigger allocation"

      genStepPrototype :: C.Definition
      genStepPrototype =
          [cedecl| void $id:step_proc($ty:struct_ssm_act *$id:act); |]

-- compile expressions

cast :: C.Exp -> MUExp -> C.Exp
cast e me = [cexp| ($ty:(baseType (IR.expType me))) $exp:e |]

compileExp :: [Reference] -> MUExp -> C.Exp
compileExp localrefs (Marshal e)
  | isHeapAllocated (IR.expType e) = [cexp| $id:ssm_new_time($exp:(compileExp localrefs e)) |]
  | otherwise                      = [cexp| $id:marshal($exp:(compileExp localrefs e)) |]
compileExp localrefs (Unmarshal e)
  | isHeapAllocated (IR.expType e) = cast [cexp| $id:ssm_time_read( $exp:(compileExp localrefs e)) |] e
  | otherwise                      = cast [cexp| $id:(ID.unmarshal)($exp:(compileExp localrefs e)) |] e
compileExp localrefs (Var t id)       = [cexp| $id:cont->$id:id |]
compileExp localrefs (Lit t l)        = compileLit localrefs l
compileExp localrefs (UOpE t e op)    = compileUOpE localrefs e op
compileExp localrefs (UOpR t r op)    = compileUOpR localrefs r op
compileExp localrefs (BOp t e1 e2 op) = compileBinOp localrefs e1 e2 op

compileLit :: [Reference] -> SSMLit -> C.Exp
compileLit localrefs (LUInt8 w)  = [cexp| $uint:w |]
compileLit localrefs (LUInt32 w) = [cexp| $uint:w |]
compileLit localrefs (LUInt64 w) = [cexp| $uint:w |]
compileLit localrefs (LInt32 i)  = [cexp| $int:i |]
compileLit localrefs (LInt64 i)  = [cexp| $int:i |]
compileLit localrefs LEvent      = [cexp| 0 |]
compileLit localrefs (LBool b)   = let lit = if b then [cexp| true|] else [cexp|false|]
                                   in [cexp| $exp:lit |]

compileUOpE :: [Reference] -> MUExp -> UnaryOpE -> C.Exp
compileUOpE localrefs e Neg = [cexp| - $exp:(compileExp localrefs e) |]
compileUOpE localrefs e Not = [cexp| ! $exp:(compileExp localrefs e) |]

compileUOpR :: [Reference] -> Reference -> UnaryOpR -> C.Exp
compileUOpR localrefs r op = case op of
    Changed -> [cexp| $id:(accessRef r)->last_updated == $id:ssm_now() |]
    Deref   -> [cexp| $id:ssm_deref($id:(accessRef r)) |]

compileBinOp :: [Reference] -> MUExp -> MUExp -> BinOp -> C.Exp
compileBinOp localrefs e1 e2 op = case op of
    OPlus   -> [cexp| $exp:e1' + $exp:e2' |]
    OMinus  -> [cexp| $exp:e1' - $exp:e2' |]
    OTimes  -> [cexp| $exp:e1' * $exp:e2' |]
    ODiv    -> [cexp| $exp:e1' / $exp:e2' |]
    ORem    -> [cexp| $exp:e1' % $exp:e2' |]
    OMin    -> [cexp| $exp:e1' < $exp:e2' ? $exp:e1' : $exp:e2' |]
    OMax    -> [cexp| $exp:e1' < $exp:e2' ? $exp:e2' : $exp:e1' |]
    OLT     -> [cexp| $exp:e1' < $exp:e2' |]
    OEQ     -> [cexp| $exp:e1' == $exp:e2' |]
    OAnd    -> [cexp| $exp:e1' && $exp:e2' |]
    OOr     -> [cexp| $exp:e1' || $exp:e2' |]
    OLShift -> [cexp| $exp:e1' << $exp:e2' |]
    ORShift -> [cexp| $exp:e1' >> $exp:e2' |]
    OBAnd   -> [cexp| $exp:e1' & $exp:e2' |]
    OBOr    -> [cexp| $exp:e1' | $exp:e2' |]
    OBXor   -> [cexp| $exp:e1' ^ $exp:e2' |]
  where
      e1' = [cexp| $exp:(compileExp localrefs e1) |]
      e2' = [cexp| $exp:(compileExp localrefs e2) |]

-- debug stuff

renderActStepBegin :: String -> String
renderActStepBegin routineName = show $ ActStepBegin routineName

renderSVifChanged :: Reference -> C.BlockItem
renderSVifChanged r = [citem|
    if($exp:is_not_nil && $exp:has_been_updated) {
        $item:(renderSV (refType r) (refIdent r))
    }

    |]
  where
      sv :: C.Exp
      sv = [cexp| $id:ssm_to_sv($id:cont->$id:(refName r)) |]

      is_not_nil :: C.Exp
      is_not_nil = [cexp| !$id:is_nil($id:cont->$id:(refName r)) |]

      has_been_updated :: C.Exp
      has_been_updated = [cexp| $exp:sv->last_updated != $id:ssm_never|]

renderSV :: Type -> Ident -> C.BlockItem
renderSV t id = case t of
    Ref t -> renderSV' t id $ compileExp [] theexp
    _     -> renderSV' t id $ compileExp [] theexp
  where
      theexp :: MUExp
      theexp
        | isReference t = IR.unmarshal $ UOpR (dereference t) (makeDynamicRef id t) Deref
        | otherwise     = IR.unmarshal $ Var t id

      renderSV' :: Type -> Ident -> C.Exp -> C.BlockItem
      renderSV' t id v = case t of
          TEvent -> [citem| $id:debug_trace($string:fmt); |]
          _      -> [citem| $id:debug_trace($string:fmt, $exp:v); |]
        where
            fmt :: String
            fmt = case t of
                TEvent -> show $ ActVar $ VarVal (ident id) TEvent UnitType
                _      -> show $ ActVar $ VarVal (ident id) t    $ IntegralFmt ifmt
              where
                  ifmt :: String
                  ifmt = case t of
                      TUInt8  -> "%u"
                      TUInt32 -> "%lu"
                      TUInt64 -> "%lu"
                      TInt32  -> "%d"
                      TInt64  -> "%ld"
                      TBool   -> "%u"
                      _       -> error $ "no formatter evailable for type: " <> show t
      
            -- signed :: Type -> C.Exp -> C.Exp
            -- signed t e = [cexp| ($ty:(base t)) $exp:e |]
      
            -- base :: Type -> C.Type
            -- base TUInt8  = [cty| typename uint8_t  |]
            -- base TUInt32 = [cty| typename uint32_t |]
            -- base TUInt64 = [cty| typename uint64_t |]
            -- base TInt32  = [cty| typename int32_t  |]
            -- base TInt64  = [cty| typename int64_t  |]
            -- base TBool   = [cty| typename bool     |]
            -- base _       = error "no basetype available"
