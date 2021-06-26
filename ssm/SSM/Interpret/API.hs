{-| This module exposes the API a module can communicate with in order to interpret
a Program. I've tried to make a clear line so that nothing about the internals of
the different types in SSM.Interpret.Internal leaks to the interpreter. -}
module SSM.Interpret.API
    ( -- * Interpretation monad
      Interp

      -- * Functions that help define the `interpret` function
    , mkProc
    , variableStorage
    , interpState
    , params
    , getReferences
    , emitResult
    , (<#>)

      -- * Talking about time
    , SSM.Interpret.API.now
    , setNow

      -- * Interacting with the event queue
    , eventQueueSize
    , eventQueueEmpty
    , nextEventTime
    , performEvents
    , schedule_event

      -- * Interacting with the ready queue
    , enqueue
    , dequeue
    , contQueueSize
    , currentProcess
    , setCurrentProcess

      -- * Instruction management
    , nextInstruction
    , pushInstructions

      -- * Interacting with references
    , newRef
    , writeRef
    , readRef
    , newVar

      -- * Sensitizing a process
    , wait

      -- * Forking processes
    , fork
    , setRunningChildren
    , addressToSelf
    , pds

      -- * Leaving a process
    , leave

-- * Evaluating an expression
    , eval

      -- * Escape hatch to go from the embedded expressions to Haskell expressions
    , getInt32
    , getInt64
    , getUInt8
    , getUInt64
    , getBool
    ) where

import SSM.Util.HughesList
import SSM.Interpret.Internal
--import SSM.Interpret.Exp
import qualified SSM.Interpret.Trace as T
import SSM.Core.LowSyntax

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.STRef.Lazy
import Data.Int
import Data.Word
import Data.Maybe
import Data.List

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.ST.Lazy

-- | Create a variable with an initial value
createVar :: SSMExp -> Interp s (Var s)
createVar e = do
    v <- eval e
    now' <- gets SSM.Interpret.Internal.now
    lift' $ newVar' v now'

{- | Create a new reference with an initial value, and adds it to the current process's
variable storage. It is considered written to when it is created. -}
newRef :: Name -> SSMExp -> Interp s ()
newRef n e = do
    ref <- createVar e
    p <- gets process
    modify $ \st -> st { process = p { localrefs = Map.insert (getVarName n) ref (localrefs p) } }

-- | Create a new variable with an initial value, and adds it to the current process's
-- variable storage. When a variable is created it is considered written to.
newVar :: Name -> SSMExp -> Interp s ()
newVar n e = do
    ref <- createVar e
    p <- gets process
    modify $ \st -> st { process = p { variables = Map.insert (getVarName n) ref (variables p) } }

-- | Write a value to a reference.
writeRef :: String -> SSMExp -> Interp s ()
writeRef r e = do
    p <- gets process
    case Map.lookup r (variables p) of
        Just ref -> do v <- eval e
                       writeVar ref v
        Nothing  -> case Map.lookup r (localrefs p) of
            Just ref -> do v <- eval e
                           writeVar ref v
            Nothing -> error $ "interpreter error - can not find variable " ++ r

-- | Read a variable from the current processes environment.
readRef :: Reference -> Interp s SSMExp
readRef r = do
    r <- lookupRef (refName r)
    (vr,_,_,_,_) <- lift' $ readSTRef r
    lift' $ readSTRef vr

currentProcess :: Interp s (Proc s)
currentProcess = gets process

-- | Return the next instruction of the current process if one exists.
nextInstruction :: Interp s (Maybe Stm)
nextInstruction = do
    p <- gets process
    case continuation p of
        [] -> return Nothing
        (x:xs) -> do modify $ \st -> st { process = p { continuation = xs } }
                     return $ Just x

-- | Push additional instructions onto a process. Used e.g when evaluating
-- an if statement where one of the branches should be evaluated.
pushInstructions :: [Stm] -> Interp s ()
pushInstructions stmts = do
    p <- gets process
    modify $ \st -> st { process = p { continuation = stmts ++ continuation p } }

wait :: [Reference] -> Interp s ()
wait refs = do
    refs' <- mapM (lookupRef . fst) refs
    modify $ \st -> st { process = (process st) { waitingOn = Just refs' } }
    mapM_ (sensitize . fst) refs

setRunningChildren :: Int -> Interp s ()
setRunningChildren n =
    modify $ \st -> st { process = (process st) { runningChildren = n } }

--addressToSelf :: Interp s ()
addressToSelf :: Interp s (STRef s (Proc s))
addressToSelf = do
    p <- gets process
    lift' $ newSTRef p

-- | Enqueue a new, forked process.
fork :: (String, [Either SSMExp Reference])  -- ^ Procedure to fork (name and arguments)
     -> Int                                  -- ^ Priority of the new process
     -> Int                                  -- ^ Depth of the new process
     -> STRef s (Proc s)                     -- ^ Reference to the parent process
     -> Interp s ()
fork (n,args) prio dep par = do
    p <- lookupProcedure n
    variables <- params $ (fst . unzip . arguments) p
    enqueue $ mkProc prio dep 0 (Just par) variables Map.empty Nothing (body p)
  where
      -- | Return an initial variable storage for the new process. Expression arguments are turned into
      -- new STRefs while reference arguments are passed from the calling processes variable storage.
      params :: [String] -> Interp s (Map.Map String (Var s))
      params names = do
          st <- gets process
          currenttime <- gets SSM.Interpret.Internal.now
          m <- flip mapM (zip names args) $ \(n, a) ->
              case a of
                  Left e  -> do v <- eval e
                                v' <- lift' (newVar' v currenttime)
                                return (n, v')
                  Right r -> do ref <- lookupRef (fst r)
                                return (n, ref)
          return $ Map.fromList m

leave :: Interp s ()
leave = do
    p <- gets process

    -- need to dequeue event on local references before we leave
    let lrefs = Map.elems $ localrefs p
    todeq <- flip filterM lrefs $ \r -> do   -- references to dequeue
        (_,_,_,mt,_) <- lift' $ readSTRef r
        return $ isJust mt
    todeqpairs <- flip mapM todeq $ \r -> do -- turn them into pairs
        (_,_,_,mt,_) <- lift' $ readSTRef r
        return (fromJust mt, r)

    modify $ \st -> st { events = delete_events todeqpairs (events st)
                       , numevents = numevents st - length todeq
                       }
    
    -- if we have a parent and we are the only running child, schedule the parent
    case parent p of
        Nothing  -> return ()
        Just p' -> do
            par' <- lift' $ readSTRef p'
            if runningChildren par' == 1
                then enqueue $ par' { runningChildren = 0 }
                else lift' $ writeSTRef p' $ par' { runningChildren = runningChildren par' - 1 }

{- | Perform all the events scheduled for this instance, enqueueing those processes that
were waiting for one of these events to happen. -}
performEvents :: Interp s ()
performEvents = do
    es <- currentEvents
    mapM_ performEvent es
  where
      -- | Fetch the events at this instant in time, if any
      currentEvents :: Interp s [Var s]
      currentEvents = do
          st <- get
          n <- gets SSM.Interpret.Internal.now
          let current = Map.lookup n (events st)
          let future  = Map.delete n (events st)

          put $ st { events    = future
                   , numevents = numevents st - maybe 0 length current
                   }
          return $ maybe [] reverse current

      {- | Perform the update of a scheduled event and enqueue processes that were waiting for
      this event to happen. -}
      performEvent :: Var s -> Interp s ()
      performEvent e = do
          st <- get

          -- fetch the variable information and reset the event fields
          (r,procs,b,me,mv) <- lift' $ readSTRef e
          lift' $ writeSTRef e (r,procs,b,Nothing,Nothing)
          
          -- output event information to trace
          tell $ toHughes [T.Event (SSM.Interpret.Internal.now st) (fromJust mv)]
          
          -- perform the actual update, eventually scheduling processes
          writeVar_ e (fromJust mv) (-1)

schedule_event :: Reference -> Word64 -> SSMExp -> Interp s ()
schedule_event r thn val = do
    st <- get

    e <- lookupRef (refName r)
    
    when (SSM.Interpret.Internal.now st > thn) $ error "bad after"

    -- fetch ref so we can update the scheduled information
    (ref,pr,b,mt,_) <- lift' $ readSTRef e
    lift' $ writeSTRef e (ref,pr,b,Just thn, Just val)
    
    -- if it was scheduled before we remove the old one from the eventqueue
    -- and just insert it again.
    if isJust mt
        then do let newevs = insert_event thn e (delete_event (fromJust mt) e (events st))
                modify $ \st -> st { events = newevs }
    -- otherwise we check if the queue is full before we schedule the new event
        else if numevents st == eventqueueSize
            then error "eventqueue full"
            else do let es' = insert_event thn e (events st)
                    modify $ \st -> st { events    = es'
                                       , numevents = numevents st + 1
                                       }
  where
      insert_event :: Word64 -> Var s -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
      insert_event when v m = adjustWithDefault (v :) [v] when m
        where
            adjustWithDefault :: Ord k => (a -> a) -> a -> k -> Map.Map k a -> Map.Map k a
            adjustWithDefault f v k m =
                if Map.member k m
                    then Map.adjust f k m
                    else Map.insert k v m

delete_event :: Word64 -> Var s -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
delete_event when v m = case Map.lookup when m of
    Just [x] -> if x == v then Map.delete when m else m
    Just x   -> Map.adjust (delete v) when m
    Nothing  -> m

delete_events :: [(Word64, Var s)] -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
delete_events [] m         = m
delete_events ((t,v):xs) m = delete_events xs $ delete_event t v m

-- | Inspects the eventqueue and returns the next event time.
nextEventTime :: Interp s Word64
nextEventTime = do
    evs <- gets events
    if Map.null evs
        then return maxBound
        else return $ fst $ Map.findMin evs

eventQueueEmpty :: Interp s Bool
eventQueueEmpty = do
    evs <- gets events
    return $ Map.null evs

eventQueueSize :: Interp s Int
eventQueueSize = gets numevents

contQueueSize :: Interp s Int
contQueueSize = gets numconts

-- | Get the current time
now :: Interp s Word64
now = gets SSM.Interpret.Internal.now

setNow :: Word64 -> Interp s ()
setNow w = modify $ \st -> st { SSM.Interpret.Internal.now = w }

setCurrentProcess :: Proc s -> Interp s ()
setCurrentProcess p = modify $ \st -> st { process = p }

-- | Evaluate an SSM expression.
eval :: SSMExp -> Interp s SSMExp
eval e = do
    p <- gets process
    case e of
        Var _ n -> case Map.lookup n (variables p) of
            Just r -> do 
                v <- lift $ lift $ (readSTRef . \(x,_,_,_,_) -> x) =<< readSTRef r
                eval v
            Nothing -> error $ "interpreter error - variable " ++ n ++ " not found in current process"
        Lit _ l -> return e
        UOpR _ r op -> case op of
            Changed -> wasWritten $ fst r
        UOpE _ e Neg -> do
            e' <- eval e
            return $ neg e'
        BOp TBool e1 e2 op -> do
            l1 <- eval e1
            l2 <- eval e2
            case op of
                OLT -> return $ lessthan l1 l2
                OEQ -> return $ equals l1 l2
        BOp _ e1 e2 op -> do
            i1 <- eval e1
            i2 <- eval e2
            case op of
                OPlus  -> return $ addition i1 i2
                OMinus -> return $ SSM.Interpret.API.subtract i1 i2
                OTimes -> return $ multiply i1 i2

neg :: SSMExp -> SSMExp
neg (Lit _ (LInt32 i)) = Lit TInt32 $ LInt32 (-i)
neg (Lit _ (LInt64 i)) = Lit TInt64 $ LInt64 (-i)

lessthan :: SSMExp -> SSMExp -> SSMExp
lessthan (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TBool $ LBool $ i1 < i2
lessthan _ _ = error "can only order numerical values"

equals :: SSMExp -> SSMExp -> SSMExp
equals (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LBool b1))  (Lit _ (LBool b2))    = Lit TBool $ LBool $ b1 == b2

addition :: SSMExp -> SSMExp -> SSMExp
addition (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 + i2
addition (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 + i2
addition (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 + i2
addition (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 + i2
addition _ _ = error "can only add numerical values"

subtract :: SSMExp -> SSMExp -> SSMExp
subtract (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 - i2
subtract (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 - i2
subtract (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 - i2
subtract (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 - i2
subtract _ _ = error "can only subtract numerical values"

multiply :: SSMExp -> SSMExp -> SSMExp
multiply (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 * i2
multiply (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 * i2
multiply (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 * i2
multiply (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 * i2
multiply _ _ = error "can only multiply numerical values"

{- | Retrieve a Haskell Int32 from an expression. Will crash if the expression
is not an Int32. -}
getInt32 :: SSMExp -> Int32
getInt32 (Lit _ (LInt32 i)) = i
getInt32 e                = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Int64 from an expression. Will crash if the expression
is not an Int64. -}
getInt64 :: SSMExp -> Int64
getInt64 (Lit _ (LInt64 i)) = i
getInt64 e                  = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Word8 from an expression. Will crash if the expression
is not an Word8. -}
getUInt8 :: SSMExp -> Word8
getUInt8 (Lit _ (LUInt8  i)) = i
getUInt8 e                   = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Word64 from an expression. Will crash if the expression
is not an Word64. -}
getUInt64 :: SSMExp -> Word64
getUInt64 (Lit _ (LUInt64 i)) = i
getUInt64 e                   = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Bool from an expression. Will crash if the expression
is not an Bool. -}
getBool :: SSMExp -> Bool
getBool (Lit _ (LBool b)) = b
getBool e                 = error $ "not a boolean: " ++ show e


-- The below functions are helper functions for writing the main interpret function

-- | Creates the initial variable storage for a program. Expressions are just
-- allocated in an STRef, while references are given a default value and then
-- allocated in an STRef.
params :: Program -> ST s (Map.Map String (Var s))
params p = do
    process <- case Map.lookup (entry p) (funs p) of
        Just p' -> return p'
        Nothing -> error $ concat ["interpreter error - can not find function ", entry p]
    m <- flip mapM (zip (arguments process) (args p)) $ \((n,t), a) ->
        case a of
            Left e  -> do
                v <- newVar' e 0
                return (n,v)
            Right r -> do
                v <- newVar' (defaultValue (dereference t)) 0
                return (n,v)
    return $ Map.fromList m
  where
      -- | Default values for SSM types.
      defaultValue :: Type -> SSMExp
      defaultValue TInt32  = Lit TInt32 $ LInt32 0
      defaultValue TInt64  = Lit TInt64 $ LInt64 0
      defaultValue TUInt64 = Lit TUInt64 $ LUInt64 0
      defaultValue TUInt8  = Lit TUInt8 $ LUInt8 0
      defaultValue TBool   = Lit TBool $ LBool False
      defaultValue (Ref _) = error "default value of reference not allowed here"

getReferences :: Program -> Map.Map String (Var s) -> [(String, Var s)]
getReferences p m = case Map.lookup (entry p) (funs p) of
    Just pr -> let refparams   = filter (\(_,t) -> isReference t) $ arguments pr
                   paramnames = fst $ unzip refparams
                   vars       = map (\n -> (n, Map.lookup n m)) paramnames
                   vars'      = filter (\(_,may) -> isJust may) vars
                in map (\(n,t) -> (n, fromJust t)) vars'
    Nothing -> error $ "interpreter error - robert did something very wrong"

-- | Emits the result of the input reference variables at the end of the program.
emitResult :: Interp s ()
emitResult = do
    refs <- gets inputargs
    forM_ refs $ \(n,v) -> do
        (ref,_,_,_,_) <- lift' $ readSTRef v
        val <- lift' $ readSTRef ref
        tell $ toHughes [T.Result n val]

-- | Infix operator for applying a unlifted argument to a lifted function.
-- Surprised that I did not find this anywhere.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
fa <#> b = fa <*> pure b
infixl 4 <#>