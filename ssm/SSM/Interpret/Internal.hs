module SSM.Interpret.Internal
    ( -- * Interpretation types
      Var
    , Proc(..)
    , Interp
    , St(..)
    , mkProc
    , variableStorage
    , interpState
    , lift'

      -- * Var management
    , newVar'
    , writeVar
    , writeVar_
    , lookupRef
    , wasWritten

      -- * Ready queue management
    , enqueue
    , dequeue

      -- * Event management
    , schedule_event
    , delete_events

      -- * Blocking calls
    , sensitize

      -- * Procedure management
    , pds
    , lookupProcedure
      ) where

import Data.STRef.Lazy
import Data.Word
import Data.Maybe
import Data.List

import SSM.Core.LowSyntax
import SSM.Util.HughesList

import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified SSM.Interpret.Trace as T

type Var s = STRef s
  ( -- Reference pointing to the actual value of the variable.
    STRef s SSMExp
  , -- List of processes that are waiting for writes to this variable.
    Map.Map Int (Proc s)
  , -- The time when this variable was last written to
    Word64
    -- The next time there's a scheduled event on this variable
  , Maybe Word64
    -- The new value this variable will get at the next event time
  , Maybe SSMExp
  )

-- | State of a single process. Equivalent to the struct in C.
data Proc s = Proc
  { -- | priority of the process
    priority        :: Int
    -- | The depth, which helps give priorities to children
  , depth           :: Int
  -- | Number of non-terminated child processes
  , runningChildren :: Int
  -- | Parent of this process, Nothing in the case of main
  , parent          :: Maybe (STRef s (Proc s))
  -- | Variables that are either expressions or references passed from a parent
  , variables       :: Map.Map String (Var s)
  -- | Variables that are references that are local to this process
  , localrefs       :: Map.Map String (Var s)
  -- | Variables this process is waiting for, if any
  , waitingOn       :: Maybe [Var s]
  -- | The work left to do for this process
  , continuation    :: [Stm]
  }
  deriving Eq

instance Show (Proc s) where
    show p = show $ priority p

instance Ord (Proc s) where
    p1 <= p2 = priority p1 <= priority p2

-- | The interpreter state.
data St s = St
  { -- | Current time
    now        :: Word64
  -- | Outstanding events
  , events     :: Map.Map Word64 [Var s]
  -- | Number of outstanding events
  , numevents  :: Int
  -- | Processes ready to run, should be a priority queue
  , readyQueue :: IntMap.IntMap (Proc s)
  -- | Number of processes in the readyqueue
  , numconts   :: Int
  -- | Map that associated names with procedures.
  , procedures :: Map.Map String Procedure
  -- | Argment-references given to the entrypoint
  , inputargs  :: [(String, Var s)]
  -- | Currently running process
  , process    :: Proc s
  }
  deriving Eq

{- | Alias for creating a process, so we don't expose the internals of the Proc s
datatype. -}
mkProc :: Int                       -- ^ Priority
       -> Int                       -- ^ Depth
       -> Int                       -- ^ #Running children
       -> Maybe (STRef s (Proc s))  -- ^ Reference to parent, if any
       -> Map.Map String (Var s)    -- ^ Variable storage
       -> Map.Map String (Var s)    -- ^ Local reference storage
       -> Maybe [Var s]             -- ^ Variables to wait for
       -> [Stm]                     -- ^ Continuation
       -> Proc s
mkProc = Proc

{- | Alias for getting the variable storage from a process, so we don't expose the
internals of the Proc s datatype. -}
variableStorage :: Proc s -> Map.Map String (Var s)
variableStorage = variables

{- | Alias for creating the interpretation state, so that we don't expose the internals
of the St type to the developer. -}
interpState :: Word64                    -- ^ Now
            -> Map.Map Word64 [Var s]    -- ^ Events
            -> Int                       -- ^ #numevents
            -> IntMap.IntMap (Proc s)    -- ^ Ready queue
            -> Int                       -- ^ #numcontinuations
            -> Map.Map String Procedure  -- ^ Procedures
            -> [(String, Var s)]         -- ^ Input references
            -> Proc s                    -- ^ Current process
            -> St s
interpState = St

eventqueueSize :: Int
eventqueueSize = 256

contqueueSize :: Int
contqueueSize = 128

type Interp s a = StateT (St s) (WriterT (Hughes T.OutputEntry) (ST s)) a

lift' :: ST s a -> Interp s a
lift' = lift . lift

-- | Function returns True if variable was written in this instant, and otherwise False.
wasWritten :: String -> Interp s SSMExp
wasWritten r = do
    p <- gets process
    n <- gets now
    case Map.lookup r (variables p) of
        Just v -> do (_,_,t,_,_) <- lift' $ readSTRef v
                     return $ Lit TBool $ LBool $ t == n
        Nothing -> case Map.lookup r (localrefs p) of
            Just v  -> do (_,_,t,_,_) <- lift' $ readSTRef v
                          return $ Lit TBool $ LBool $ t == n
            Nothing -> error $ "interpreter error - can not find variable " ++ r

-- | Creates a new `Var s` with an initial value.
newVar' :: SSMExp -> Word64 -> ST s (Var s)
newVar' v n = do
    val <- newSTRef v
    ref <- newSTRef (val, Map.empty, n, Nothing, Nothing)
    return ref

writeVar :: Var s -> SSMExp -> Interp s ()
writeVar ref e = do
    p <- gets process
    writeVar_ ref e (priority p)

writeVar_ :: Var s -> SSMExp -> Int -> Interp s ()
writeVar_ ref e prio = do
    (variable,waits,_,me,mv) <- lift' $ readSTRef ref
    lift' $ writeSTRef variable e -- actually update the variable value

    let (keep, towake) = Map.split prio waits

    -- wake up and desensitize the processes
    mapM_ desensitize towake

    -- update the variable to be written to in this instant and give it knowledge of
    -- which processes are still waiting on it
    n <- gets now
    lift' $ writeSTRef ref (variable, keep, n, me, mv)
  where
      desensitize :: Proc s -> Interp s ()
      desensitize p = do
          let variables = fromJust $ waitingOn p
          forM_ variables $ \r -> do
              (ref,procs,b,me,mv) <- lift' $ readSTRef r
              lift' $ writeSTRef r (ref, Map.delete (priority p) procs,b,me,mv)
          enqueue $ p { waitingOn = Nothing }

-- | Enqueue a process in the ready queue, ordered by its priority
enqueue :: Proc s -> Interp s ()
enqueue p = do
    nc <- gets numconts
    if nc >= contqueueSize
        then error "contqueue full"
        else modify $ \st -> st { readyQueue = IntMap.insert (priority p) p (readyQueue st)
                                , numconts   = numconts st + 1
                                }

-- | Fetch the process with the lowest priority from the ready queue
dequeue :: Interp s (Proc s)
dequeue = do
    st <- get
    let conts = readyQueue st
    if IntMap.null conts
        then error "interpreter error - dequeue called on empty readyqueue"
        else do let x = IntMap.findMin conts
                let conts' = IntMap.deleteMin conts
                put $ st { readyQueue = conts'
                         , numconts = numconts st - 1
                         }
                return $ snd x

-- | Look up a variable in the currently running process variable store.
lookupRef :: String -> Interp s (Var s)
lookupRef r = do
    p <- gets process
    case Map.lookup r (variables p) of
      Just ref -> return ref
      Nothing  -> case Map.lookup r (localrefs p) of
          Just ref -> return ref
          Nothing -> error $ "interpreter error - can not find variable " ++ r

schedule_event :: Reference -> Word64 -> SSMExp -> Interp s ()
schedule_event r thn val = do
    st <- get

    e <- lookupRef (refName r)
    
    when (now st > thn) $ error "bad after"

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

-- | Make the procedure wait for writes to the variable
sensitize :: String -> Interp s ()
sensitize v = do
    p <- gets process
    r <- lookupRef v
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
    let prio  = priority p                                  -- old prio
    let dep   = depth p                                     -- old dep
    let d'    = dep - ceiling (logBase 2 (fromIntegral k))  -- new dep
    if d' < 0
        then error "negative depth"
    else do let prios = [ prio + i * (2^d') | i <- [0..k-1]]        -- new prios
            return $ zip prios (repeat d')

-- | Simple lookup function that throws an error if the desired procedure does not exist in
-- the procedure storage.
lookupProcedure :: String -> Interp s Procedure
lookupProcedure n = do
    st <- get
    case Map.lookup n (procedures st) of
        Just p -> return p
        Nothing -> error $ "interpreter error - trying to fork non-existant procedure"