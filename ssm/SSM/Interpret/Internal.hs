-- | Helper functions and auxiliary definitions the interpreter uses.
module SSM.Interpret.Internal
  ( -- * Interpretation monad, re-exported from "SSM.Interpret.Types".
    Interp
  , InterpretConfig(..)
  , terminate
  , crash
  , microtick
  , tellEvent
  , traceVars

  -- * Interpreter state helpers
  , mkProc
  , procName
  , variableStorage
  , initState
  , globals

  -- * Model time helpers
  , getNow
  , setNow

  -- * Event queue management helpers
  , eventQueueSize
  , eventQueueEmpty
  , nextEventTime
  , performEvents
  , scheduleEvent

  -- * Activation queue helpers
  , enqueue
  , dequeue
  , actQueueSize
  , currentProcess
  , setCurrentProcess

  -- * Instruction management helpers
  , nextInstruction
  , pushInstructions

  -- * Reference helpers
  , newRef
  , writeRef
  , writeLocal
  , readRef

  -- * Wait, fork, and leave + helpers
  , wait
  , fork
  , setRunningChildren
  , addressToSelf
  , pds
  , leave

  -- * Evaluation expressions
  , eval
  , getInt32
  , getInt64
  , getUInt8
  , getUInt64
  , getBool
  , genTimeDelay
  ) where



import           Control.Monad.ST.Lazy          ( ST )
import           Control.Monad.State.Lazy       ( MonadState(..)
                                                , MonadTrans(..)
                                                , filterM
                                                , forM
                                                , forM_
                                                , gets
                                                , modify
                                                , when
                                                )
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import qualified Data.IntMap                   as IntMap
import           Data.List                      ( delete
                                                , sortOn
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import           Data.STRef.Lazy                ( STRef
                                                , newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import           Data.Word                      ( Word64
                                                , Word8
                                                )
import           SSM.Util.HughesList            ( toHughes )
import           SSM.Util.Operators             ( (<#>) )

import           SSM.Core.Syntax
import qualified SSM.Interpret.Trace           as T
import           SSM.Interpret.Types

{-********** Main interpret function helpers **********-}

-- | Create initial global variable storage
globals :: Program -> ST s (Map.Map Ident (Var s))
globals p = do
  vars <- forM (globalReferences p) $ \(id,t) -> do
    let initval = defaultValue (dereference t)
    v <- newVar' initval maxBound
    return (id, v)
  return $ Map.fromList vars

-- | Default values for SSM types.
defaultValue :: Type -> SSMExp
defaultValue TInt32  = Lit TInt32 $ LInt32 0
defaultValue TInt64  = Lit TInt64 $ LInt64 0
defaultValue TUInt8  = Lit TUInt8 $ LUInt8 0
defaultValue TUInt32 = Lit TUInt32 $ LUInt32 0
defaultValue TUInt64 = Lit TUInt64 $ LUInt64 0
defaultValue TBool   = Lit TBool $ LBool False
defaultValue TEvent  = Lit TEvent LEvent
defaultValue (Ref _) = error "default value of reference not allowed"

-- | Log the state of all variables in the current process to the event trace.
traceVars :: Interp s ()
traceVars = do
  mapM_ varEvent . sortOn fst . Map.toList . variables =<< currentProcess
  mapM_ varEvent . sortOn fst . Map.toList . localrefs =<< currentProcess
 where
  varEvent (n, v) = do
    (e, _, _, _, _) <- lift' $ readSTRef v
    (t, v)          <- getTypeConcreteVal <$> lift' (readSTRef e)
    tellEvent $ T.ActVar $ T.VarVal (identName n) t v

{-********** Time management **********-}

-- | Get the current time.
getNow :: Interp s Word64
getNow = gets SSM.Interpret.Types.now

-- | Set the current time.
setNow :: Word64 -> Interp s ()
setNow w = modify $ \st -> st { SSM.Interpret.Types.now = w }


{-********** Interacting with the event queue **********-}

{- | Schedule a delayed update of a reference.

The reference @r@ will get the value @val@ in @thn@ units of time.
-}
scheduleEvent :: Reference -> Word64 -> SSMExp -> Interp s ()
scheduleEvent r thn val = do
  st <- get
  e  <- lookupRef r

  when (SSM.Interpret.Types.now st > thn) $ terminate T.CrashInvalidTime

  -- Fetch ref so we can update the scheduled information
  (ref, pr, b, mt, _) <- lift' $ readSTRef e
  lift' $ writeSTRef e (ref, pr, b, Just thn, Just val)

  -- If it was scheduled before we remove the old one from the eventqueue
  -- and just insert it again.
  case mt of
    Just t -> modify
      $ \st -> st { events = insertEvent thn e (deleteEvent t e (events st)) }
    Nothing -> do
      meqs <- eventqueueSize
      when (numevents st == meqs) $ terminate T.ExhaustedEventQueue
      modify $ \st -> st { events    = insertEvent thn e (events st)
                         , numevents = numevents st + 1
                         }
 where
  insertEvent
    :: Word64 -> Var s -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
  insertEvent when v m = adjustWithDefault (v :) [v] when m
   where
    adjustWithDefault
      :: Ord k => (a -> a) -> a -> k -> Map.Map k a -> Map.Map k a
    adjustWithDefault f v k m =
      if Map.member k m then Map.adjust f k m else Map.insert k v m

deleteEvent
  :: Word64 -> Var s -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
deleteEvent when v m = case Map.lookup when m of
  Just [x] -> if x == v then Map.delete when m else m
  Just x   -> Map.adjust (delete v) when m
  Nothing  -> m

deleteEvents
  :: [(Word64, Var s)] -> Map.Map Word64 [Var s] -> Map.Map Word64 [Var s]
deleteEvents []            m = m
deleteEvents ((t, v) : xs) m = deleteEvents xs $ deleteEvent t v m

-- | Inspects the eventqueue and returns the next event time.
nextEventTime :: Interp s Word64
nextEventTime = do
  evs <- gets events
  if Map.null evs then return maxBound else return $ fst $ Map.findMin evs

-- | Returns @True@ in the Interpretation monad if the event queue is empty.
eventQueueEmpty :: Interp s Bool
eventQueueEmpty = do
  evs <- gets events
  return $ Map.null evs

-- | Returns the size of the event queue.
eventQueueSize :: Interp s Int
eventQueueSize = gets numevents

-- | Upper bound on the number of events that may be scheduled at any given time.
eventqueueSize :: Interp s Int
eventqueueSize = gets maxEventQueueSize

{- | Perform all the events scheduled for this instance, enqueueing those processes that
are waiting for one of these events to happen. -}
performEvents :: Interp s ()
performEvents = do
  es <- currentEvents
  mapM_ performEvent es
 where
  -- | Fetch the events at this instant in time, if any
  currentEvents :: Interp s [Var s]
  currentEvents = do
    st <- get
    n  <- getNow
    let current = Map.lookup n (events st)
    let future  = Map.delete n (events st)

    put $ st { events    = future
             , numevents = numevents st - maybe 0 length current
             }
    return $ maybe [] reverse current

  -- | Perform the update of a scheduled event and enqueue waiting processes.
  performEvent :: Var s -> Interp s ()
  performEvent e = do
    -- Fetch the variable information and reset the event fields
    (r, procs, b, me, mv) <- lift' $ readSTRef e
    lift' $ writeSTRef e (r, procs, b, Nothing, Nothing)

    -- Perform the actual update, eventually scheduling processes
    writeVar_ e (fromJust mv) (-1)

{-********** Interacting with the ready queue **********-}

-- | Enqueue a process in the ready queue, ordered by its priority
enqueue :: Proc s -> Interp s ()
enqueue p = do
  nc   <- gets numacts
  mcqs <- gets maxActQueueSize
  if nc >= mcqs
    then terminate T.ExhaustedActQueue
    else modify $ \st -> st
      { readyQueue = IntMap.insert (priority p) p (readyQueue st)
      , numacts    = numacts st + 1
      }

-- | Fetch the process with the lowest priority from the ready queue.
dequeue :: Interp s (Proc s)
dequeue = do
  st <- get
  let acts = readyQueue st
  when (IntMap.null acts)
    $ crash "Interpreter error: dequeue called on empty readyqueue"
  put $ st { readyQueue = IntMap.deleteMin acts, numacts = numacts st - 1 }
  return $ snd $ IntMap.findMin acts

-- | Size of the ready queue.
actQueueSize :: Interp s Int
actQueueSize = gets numacts

-- | Set the current process.
setCurrentProcess :: Proc s -> Interp s ()
setCurrentProcess p = modify $ \st -> st { process = p }

-- | Get the current process.
currentProcess :: Interp s (Proc s)
currentProcess = gets process

{-********** Instruction management **********-}

{- | Return the next instruction of the current process, if one exists

Nothing will be returned if the process has no more work to do.
-}
nextInstruction :: Interp s (Maybe Stm)
nextInstruction = do
  p <- gets process
  case continuation p of
    []       -> return Nothing
    (x : xs) -> do
      modify $ \st -> st { process = p { continuation = xs } }
      return $ Just x

-- | Push additional instructions onto a process. Used e.g when evaluating
-- an if statement, where one of the branches should be evaluated.
pushInstructions :: [Stm] -> Interp s ()
pushInstructions stmts = do
  p <- gets process
  modify $ \st -> st { process = p { continuation = stmts ++ continuation p } }

{-********** Variable management **********-}

-- | Create a variable with an initial value
createVar :: SSMExp -> Interp s (Var s)
createVar e = do
  v <- eval e
  n <- getNow
  lift' $ newVar' v n

{- | Create a new reference with an initial value.

Note: it is added to the map containing the local variables. -}
newRef :: Ident -> SSMExp -> Interp s ()
newRef n e = do
    ref <- createVar e
    p <- gets process
    modify $ \st -> st { process = p { localrefs = Map.insert n ref (localrefs p) } }

{- | Create a new variable with an initial value, and adds it to the current process's
variable storage. When a variable is created it is considered written to.

Note: This does the same thing as `NewRef`, but it adds the reference to the map
containing expression variables & references supplied by a caller. -}
newVar :: Ident -> SSMExp -> Interp s ()
newVar n e = do
    ref <- createVar e
    p <- gets process
    modify $ \st -> st { process = p { variables = Map.insert n ref (variables p) } }

-- | Write a value to a variable.
writeRef :: Reference -> SSMExp -> Interp s ()
writeRef r e = do
    p <- gets process
    if isDynamic r

        -- dynamic references must be fetched from the curent context
        then case Map.lookup (refIdent r) (variables p) of
            Just ref -> do v <- eval e
                           writeVar ref v
            Nothing  -> case Map.lookup (refIdent r) (localrefs p) of
                Just ref -> do v <- eval e
                               writeVar ref v
                Nothing -> error $ "interpreter error - can't find variable " ++
                                   refName r

        -- static references can always be read from the global state
        else do globals <- gets globalVariables
                case Map.lookup (refIdent r) globals of
                    Just ref -> eval e >>= \v -> writeVar ref v
                    Nothing  -> error $ "interpreter error - can't find global variable :"
                                     ++ refName r

-- | Write a value to a local expression variable.
writeLocal :: Ident -> SSMExp -> Interp s ()
writeLocal n e = do
    p <- gets process
    case Map.lookup n (variables p) of
        Just ref -> do v <- eval e
                       writeVar ref v
        Nothing  -> case Map.lookup n (localrefs p) of
            Just ref -> do v <- eval e
                           writeVar ref v
            Nothing -> error $ "interpreter error - can't find variable " ++
                               identName n

writeDynamic_ :: Ident -> SSMExp -> Interp s ()
writeDynamic_ n e = do
    p <- gets process
    case Map.lookup n (variables p) of
        Just ref -> eval e >>= \v -> writeVar ref v
        Nothing  -> case Map.lookup n (localrefs p) of
            Just ref -> eval e >>= \v -> writeVar ref v
            Nothing  -> error $ "interpreter error - can't find variable " ++ identName n

-- | Read the value of a reference
readRef :: Reference -> Interp s SSMExp
readRef r = do
    r <- lookupRef r
    (vr,_,_,_,_) <- lift' $ readSTRef r
    lift' $ readSTRef vr

{- | This function returns @True@ if variable was written in this instant, and otherwise
@False@. -}
wasWritten :: Reference -> Interp s SSMExp
wasWritten r =
  if isDynamic r

    then do
        p <- gets process
        case Map.lookup (refIdent r) (variables p) of
            Just v -> wasWritten' v
            Nothing -> case Map.lookup (refIdent r) (localrefs p) of
                Just v  -> wasWritten' v
                Nothing ->
                    error $ "interpreter error - can not find variable " ++ refName r

    else do
        globs <- gets globalVariables
        case Map.lookup (refIdent r) globs of
            Just v -> wasWritten' v
            Nothing ->
                error $ "interpreter error - can not find global variable " ++ refName r
  where
      {- | Returns a `SSM.Core.Syntax.SSMExp` representing if the @Var s@ was written
      to or not in the current instant. -}
      wasWritten' :: Var s -> Interp s SSMExp
      wasWritten' v = do
          n <- getNow
          (_,_,t,_,_) <- lift' $ readSTRef v
          return $ Lit TBool $ LBool $ t == n

-- | Creates a new `Var s` with an initial value.
newVar' :: SSMExp -> Word64 -> ST s (Var s)
newVar' v n = do
  val <- newSTRef v
  newSTRef (val, Map.empty, n, Nothing, Nothing)

-- | Write a value to a variable.
writeVar :: Var s -> SSMExp -> Interp s ()
writeVar ref e = do
  p <- gets process
  writeVar_ ref e (priority p)

{- | Writes a value to a variable, with the specified priority.

The priority is used to make sure that we only enqueue those sensitized
processes that has a priority larger than the one we have as a parameter here.
-}
writeVar_ :: Var s -> SSMExp -> Int -> Interp s ()
writeVar_ ref e prio = do
  (variable, waits, _, me, mv) <- lift' $ readSTRef ref
  lift' $ writeSTRef variable e -- actually update the variable value

  let (keep, towake) = Map.split prio waits

  -- wake up and desensitize the processes
  mapM_ desensitize towake

  -- update the variable to be written to in this instant and give it knowledge
  -- of which processes are still waiting on it
  n <- getNow
  lift' $ writeSTRef ref (variable, keep, n, me, mv)
 where
  -- | Wake up a process, remove it from all trigger lists, and enqueueing it.
  desensitize :: Proc s -> Interp s ()
  desensitize p = do
    let variables = fromJust $ waitingOn p
    forM_ variables $ \r -> do
      (ref, procs, b, me, mv) <- lift' $ readSTRef r
      lift' $ writeSTRef r (ref, Map.delete (priority p) procs, b, me, mv)
    enqueue $ p { waitingOn = Nothing }

-- | Look up a variable associated with a reference
lookupRef :: Reference -> Interp s (Var s)
lookupRef ref =
  if isDynamic ref

    then do
      p <- gets process
      case Map.lookup (refIdent ref) (variables p) of
        Just ref -> return ref
        Nothing  -> case Map.lookup (refIdent ref) (localrefs p) of
          Just ref -> return ref
          Nothing  ->
            error $ "interpreter error - can not find variable " ++ refName ref

    else do
      globs <- gets globalVariables
      case Map.lookup (refIdent ref) globs of
          Just ref -> return ref
          Nothing ->
            error $ "interpreter error - can not find global variable " ++ refName ref

-- | Make a procedure wait for writes to the variable identified by the name @v@.
sensitize :: Reference -> Interp s ()
sensitize ref = do
    p <- gets process
    r <- lookupRef ref
    (ref,procs,b,me,mv) <- lift' $ readSTRef r
    -- don't want to register a process twice
    if Map.member (priority p) procs
        then return ()
        else lift' $ writeSTRef r (ref, Map.insert (priority p) p procs,b,me,mv)

-- | This function will, if told how many new processes are being forked, compute
-- new priorities and depths for them.
pds :: Int -> Interp s [(Int, Int)]
pds k = do
  p <- gets process
  let prio = priority p                                  -- old prio
      dep  = depth p                                     -- old dep
      d'   = dep - ceiling (logBase 2 (fromIntegral k))  -- new dep
  when (d' < 0) $ terminate T.ExhaustedPriority
  let prios = [ prio + i * (2 ^ d') | i <- [0 .. k - 1] ]        -- new prios
  return $ zip prios (repeat d')

{-********** Sensitizing a process **********-}

{- | This function will make sure that the current process will block until any
of the references in the list @refs@ have been written to.
-}
wait :: [Reference] -> Interp s ()
wait refs = do
  refs' <- mapM lookupRef refs
  modify $ \st -> st { process = (process st) { waitingOn = Just refs' } }
  mapM_ sensitize refs









{-********** Forking processes **********-}

-- | Set the number of running children of the current process to @n@.
setRunningChildren :: Int -> Interp s ()
setRunningChildren n =
  modify $ \st -> st { process = (process st) { runningChildren = n } }

-- | Get a @STRef@ which points to the current running process's activation record.
addressToSelf :: Interp s (STRef s (Proc s))
addressToSelf = do
    p <- gets process
    lift' $ newSTRef p

-- | Fork a new process.
fork :: (Ident, [Either SSMExp Reference])   -- ^ Procedure to fork (name and arguments)
     -> Int                                  -- ^ Priority of the new process
     -> Int                                  -- ^ Depth of the new process
     -> STRef s (Proc s)                     -- ^ Reference to the parent process
     -> Interp s ()
fork (n,args) prio dep par = do
    p <- lookupProcedure n
    variables <- params $ (map fst . arguments) p
    enqueue $ Proc (identName n) prio dep 0 (Just par) variables Map.empty Nothing (body p)
  where
      -- | Return an initial variable storage for the new process. Expression arguments are turned into
      -- new STRefs while reference arguments are passed from the calling processes variable storage.
      params :: [Ident] -> Interp s (Map.Map Ident (Var s))
      params names = do
          st <- gets process
          currenttime <- getNow
          m <- forM (zip names args) $ \(n, a) ->
              case a of
                  Left e  -> do v <- eval e
                                v' <- lift' (newVar' v currenttime)
                                return (n, v')
                  Right r -> do ref <- lookupRef r
                                return (n, ref)
          return $ Map.fromList m

      -- | Simple lookup function that throws an error if the desired procedure does not exist in
      -- the procedure storage.
      lookupProcedure :: Ident -> Interp s Procedure
      lookupProcedure n = do
          st <- get
          case Map.lookup n (procedures st) of
              Just p -> return p
              Nothing -> error "interpreter error - trying to fork non-existant procedure"

{-********** Process termination **********-}

{- | Terminate the current process. When the process is terminated any outstanding
events on the processes variables are deleted. If the process has a parent and the
current process is the only child, enqueue the parent process. Otherwise, make sure
that the parent process knows that one child process has terminated. -}
leave :: Interp s ()
leave = do
  p <- gets process

  -- need to dequeue event on local references before we leave
  let lrefs = Map.elems $ localrefs p
  todeq <- flip filterM lrefs $ \r -> do   -- references to dequeue
    (_, _, _, mt, _) <- lift' $ readSTRef r
    return $ isJust mt
  todeqpairs <- forM todeq $ \r -> do -- turn them into pairs
    (_, _, _, mt, _) <- lift' $ readSTRef r
    return (fromJust mt, r)

  modify $ \st -> st { events    = deleteEvents todeqpairs (events st)
                     , numevents = numevents st - length todeq
                     }

  -- if we have a parent and we are the only running child, schedule the parent
  case parent p of
    Nothing -> return ()
    Just p' -> do
      par' <- lift' $ readSTRef p'
      if runningChildren par' == 1
        then enqueue $ par' { runningChildren = 0 }
        else lift' $ writeSTRef p' $ par'
          { runningChildren = runningChildren par' - 1
          }

{-********** Expression evaluation **********-}


-- | Evaluate an @SSM@ expression
eval :: SSMExp -> Interp s SSMExp
eval e = do
  p <- gets process
  case e of
    Var _ n -> case Map.lookup n (variables p) of
      Just r -> do
        v <- lift' $ (readSTRef . \(x, _, _, _, _) -> x) =<< readSTRef r
        eval v
      Nothing -> case Map.lookup n (localrefs p) of
        Just r -> do
          v <- lift' $ (readSTRef . \(x, _, _, _, _) -> x) =<< readSTRef r
          eval v
        Nothing ->
          crash
            $  "Interpreter (eval): in process '"
            ++ procName p
            ++ "', variable not found: "
            ++ identName n
    Lit _ l     -> return e
    UOpR _ r op -> case op of
      Changed -> wasWritten r
      Deref   -> readRef r
    UOpE _ e op -> do
      e' <- eval e
      case op of
        Neg -> return $ neg e'
        Not -> return $ not' e'
    BOp TBool e1 e2 op -> do
      l1 <- eval e1
      l2 <- eval e2
      case op of
        OLT  -> return $ lessthan l1 l2
        OEQ  -> return $ equals l1 l2
        OAnd -> return $ conjunction l1 l2
        OOr  -> return $ disjunction l1 l2
        _ ->
          crash
            $  "Type error: binary operator does not return TBool: "
            ++ show op
    BOp _ e1 e2 op -> do
      i1 <- eval e1
      i2 <- eval e2
      case op of
        OPlus  -> return $ addition i1 i2
        OMinus -> return $ SSM.Interpret.Internal.subtract i1 i2
        OTimes -> return $ multiply i1 i2
        ODiv   -> return $ divide i1 i2
        ORem   -> return $ remainder i1 i2
        OMin   -> return $ min' i1 i2
        OMax   -> return $ max' i1 i2
        _ ->
          crash
            $  "Type error: binary operator does not return int type: "
            ++ show op

expOpTypeError :: String -> SSMExp -> SSMExp -> a
expOpTypeError op le re =
  error
    $  "Type error: cannot "
    ++ op
    ++ " terms of different types: "
    ++ show le
    ++ " v.s. "
    ++ show re

expTypeError :: String -> SSMExp -> a
expTypeError t e = error $ "Type error: not a " ++ t ++ ": " ++ show e

-- | Negate an expression
neg :: SSMExp -> SSMExp
neg (Lit _ (LInt32 i)) = Lit TInt32 $ LInt32 (-i)
neg (Lit _ (LInt64 i)) = Lit TInt64 $ LInt64 (-i)
neg _                  = error "can only negate signed integer types"

not' :: SSMExp -> SSMExp
not' (Lit TBool (LBool b)) = Lit TBool $ LBool $ not b
not' _                     = error "can only negate boolean expressions"

-- | Compute the ordering of an expression
lessthan :: SSMExp -> SSMExp -> SSMExp
lessthan (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TBool $ LBool $ i1 < i2
lessthan le re = expOpTypeError "compare" le re

-- | Check if two expressions are equal
equals :: SSMExp -> SSMExp -> SSMExp
equals (Lit _ (LInt32  i1)) (Lit _ (LInt32  i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LInt64  i1)) (Lit _ (LInt64  i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt8  i1)) (Lit _ (LUInt8  i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt32  i1)) (Lit _ (LUInt32  i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LBool   b1)) (Lit _ (LBool   b2)) = Lit TBool $ LBool $ b1 == b2
-- FIXME: We haven't fully fleshed out equality semantics of TEvent yet.
equals (Lit _ LEvent      ) (Lit _ LEvent      ) = Lit TBool $ LBool True
equals le re = expOpTypeError "compare" le re

conjunction :: SSMExp -> SSMExp -> SSMExp
conjunction (Lit TBool (LBool b1)) (Lit TBool (LBool b2)) = Lit TBool $ LBool $ b1 && b2
conjunction le re = expOpTypeError "conjunction" le re

disjunction :: SSMExp -> SSMExp -> SSMExp
disjunction (Lit TBool (LBool b1)) (Lit TBool (LBool b2)) = Lit TBool $ LBool $ b1 || b2
disjunction le re = expOpTypeError "disjunction"  le re

-- | Add two expressions
addition :: SSMExp -> SSMExp -> SSMExp
addition (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) =
  Lit TInt32 $ LInt32 $ i1 + i2
addition (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) =
  Lit TInt64 $ LInt64 $ i1 + i2
addition (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) =
  Lit TUInt8 $ LUInt8 $ i1 + i2
addition (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) =
  Lit TUInt32 $ LUInt32 $ i1 + i2
addition (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TUInt64 $ LUInt64 $ i1 + i2
addition le re = expOpTypeError "add" le re

-- | Subtract two expressions
subtract :: SSMExp -> SSMExp -> SSMExp
subtract (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) =
  Lit TInt32 $ LInt32 $ i1 - i2
subtract (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) =
  Lit TInt64 $ LInt64 $ i1 - i2
subtract (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) =
  Lit TUInt8 $ LUInt8 $ i1 - i2
subtract (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) =
  Lit TUInt32 $ LUInt32 $ i1 - i2
subtract (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TUInt64 $ LUInt64 $ i1 - i2
subtract le re = expOpTypeError "subtract" le re

-- | Multiply two expressions
multiply :: SSMExp -> SSMExp -> SSMExp
multiply (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) =
  Lit TInt32 $ LInt32 $ i1 * i2
multiply (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) =
  Lit TInt64 $ LInt64 $ i1 * i2
multiply (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) =
  Lit TUInt8 $ LUInt8 $ i1 * i2
multiply (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) =
  Lit TUInt32 $ LUInt32 $ i1 * i2
multiply (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TUInt64 $ LUInt64 $ i1 * i2
multiply le re = expOpTypeError "multiply" le re

divide :: SSMExp -> SSMExp -> SSMExp
divide (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) =
  Lit TInt32 $ LInt32 $ let x = i1 `div` i2 in if x < 0 then 0 else x
divide (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) =
  Lit TInt64 $ LInt64 $ let x = i1 `div` i2 in if x < 0 then 0 else x
divide (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) =
  Lit TUInt8 $ LUInt8 $ i1 `div` i2
divide (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) =
  Lit TUInt32 $ LUInt32 $ i1 `div` i2
divide (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TUInt64 $ LUInt64 $ i1 `div` i2
divide le re = expOpTypeError "divide" le re

remainder :: SSMExp -> SSMExp -> SSMExp
remainder (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) =
  Lit TInt32 $ LInt32 $ i1 `rem` i2
remainder (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) =
  Lit TInt64 $ LInt64 $ i1 `rem` i2
remainder (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) =
  Lit TUInt8 $ LUInt8 $ i1 `rem` i2
remainder (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) =
  Lit TUInt32 $ LUInt32 $ i1 `rem` i2
remainder (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) =
  Lit TUInt64 $ LUInt64 $ i1 `rem` i2
remainder le re = expOpTypeError "rem" le re

min' :: SSMExp -> SSMExp -> SSMExp
min' (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) = Lit TInt32 $ LInt32 $ min i1 i2
min' (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) = Lit TInt64 $ LInt64 $ min i1 i2
min' (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) = Lit TUInt8 $ LUInt8 $ min i1 i2
min' (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) = Lit TUInt32 $ LUInt32 $ min i1 i2
min' (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ min i1 i2
min' le re = expOpTypeError "min" le re

max' :: SSMExp -> SSMExp -> SSMExp
max' (Lit _ (LInt32 i1)) (Lit _ (LInt32 i2)) = Lit TInt32 $ LInt32 $ max i1 i2
max' (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2)) = Lit TInt64 $ LInt64 $ max i1 i2
max' (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2)) = Lit TUInt8 $ LUInt8 $ max i1 i2
max' (Lit _ (LUInt32 i1)) (Lit _ (LUInt32 i2)) = Lit TUInt32 $ LUInt32 $ max i1 i2
max' (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ max i1 i2
max' le re = expOpTypeError "max" le re

{- | Retrieve a Haskell @Int32@ from an expression. Will crash if the expression
is not an @Int32@. -}
getInt32 :: SSMExp -> Int32
getInt32 (Lit _ (LInt32 i)) = i
getInt32 e                  = expTypeError "Int32" e

{- | Retrieve a Haskell @Int64@ from an expression. Will crash if the expression
is not an @Int64@. -}
getInt64 :: SSMExp -> Int64
getInt64 (Lit _ (LInt64 i)) = i
getInt64 e                  = expTypeError "Int64" e

{- | Retrieve a Haskell @Word8@ from an expression. Will crash if the expression
is not an @Word8@. -}
getUInt8 :: SSMExp -> Word8
getUInt8 (Lit _ (LUInt8 i)) = i
getUInt8 e                  = expTypeError "Word8" e

{- | Retrieve a Haskell @Word64@ from an expression. Will crash if the expression
is not an @Word64@. -}
getUInt64 :: SSMExp -> Word64
getUInt64 (Lit _ (LUInt64 i)) = i
getUInt64 e                   = expTypeError "Word64" e

{- | Retrieve a Haskell @Bool@ from an expression. Will crash if the expression
is not an @Bool@. -}
getBool :: SSMExp -> Bool
getBool (Lit _ (LBool b)) = b
getBool e                 = expTypeError "Bool" e

-- | Recursively evaluate an @SSMTime@ value.
genTimeDelay :: SSMTime -> Interp s Word64
genTimeDelay (SSMTime d) = getUInt64 <$> eval d

-- | Obtain type and concrete representation of an expression; only works for
-- literals.
getTypeConcreteVal :: SSMExp -> (Type, T.ConcreteValue)
getTypeConcreteVal (Lit t (LInt32  i    )) = (t, T.IntegralVal $ fromIntegral i)
getTypeConcreteVal (Lit t (LInt64  i    )) = (t, T.IntegralVal $ fromIntegral i)
getTypeConcreteVal (Lit t (LUInt8  i    )) = (t, T.IntegralVal $ fromIntegral i)
getTypeConcreteVal (Lit t (LUInt32 i    )) = (t, T.IntegralVal $ fromIntegral i)
getTypeConcreteVal (Lit t (LUInt64 i    )) = (t, T.IntegralVal $ fromIntegral i)
getTypeConcreteVal (Lit t (LBool   True )) = (t, T.IntegralVal 1)
getTypeConcreteVal (Lit t (LBool   False)) = (t, T.IntegralVal 0)
getTypeConcreteVal (Lit t LEvent         ) = (t, T.UnitType)
getTypeConcreteVal e                       = expTypeError "Concrete" e
