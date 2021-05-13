module LowInterpreter where

import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Int
import Data.Word

import System.IO.Unsafe

import LowCore
import qualified Trace as T

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

type Var s = STRef s
  ( -- Reference pointing to the actual value of the variable.
    STRef s SSMExp
  , -- List of processes that are waiting for writes to this variable.
    [Proc s]
  , -- Has this variable been written to in this instance?
    Bool
  )

-- | The type of events.
data Event s = Event
  { -- | The time when this even should occur
    at  :: Word64
    -- | The reference variable that gets the new value
  , ref :: Var s
    -- | The value this reference will assume at time at
  , val :: SSMExp
  }

instance Eq (Event s) where
    e1 == e2 = ref e1 == ref e2

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
  -- | Variables and their values. In the SSM language we make a distinction between
  -- expressions and references, but in the interpreter there is no need to draw that
  -- distinction.
  , variables      :: Map.Map String (Var s)
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
  , events     :: [Event s]
  -- | Number of outstanding events
  , numevents  :: Int
  -- | Processes ready to run, should be a priority queue
  , readyQueue :: [Proc s]
  -- | References that were written in this instance
  , written    :: [Var s]
  -- | Map that associated names with procedures.
  , procedures :: Map.Map String Procedure
  -- | Argment-references given to the entrypoint
  , inputargs  :: [(String, Var s)]
  -- | Currently running process
  , process    :: Proc s
  }
  deriving Eq

type Interp s a = StateT (St s) (WriterT T.Output (ST s)) a

interpret :: Program -> T.Output
interpret p = runST interpret'
  where

      interpret' :: ST s T.Output
      interpret' = do
          fun <- case Map.lookup (main p) (funs p) of
              Just p'  -> return p'
              Nothing  -> error $ concat ["interpreter error - can not find function ", main p]
          process <- Proc 0 32 0 Nothing <$> params p <#> Nothing <#> body fun
          let refs = Map.elems $ variables process
          let actualrefs = getReferences p $ variables process
          execWriterT $ evalStateT (run >> emitResult) (St 0 [] 0 [process] refs (funs p) actualrefs process)

      -- | Creates the initial variable storage for the program. Expressions are just
      -- allocated in an STRef, while references are given a default value and then
      -- allocated in an STRef.
      params :: Program -> ST s (Map.Map String (Var s))
      params p = do
          process <- case Map.lookup (main p) (funs p) of
              Just p' -> return p'
              Nothing -> error $ concat ["interpreter error - can not find function ", main p]
          m <- flip mapM (zip (LowCore.arguments process) (args p)) $ \((n,t), a) ->
              case a of
                  Left e  -> newVar' e                >>= \v -> return (n,v)
                  Right r -> newVar' (defaultValue (dereference t)) >>= \v -> return (n,v)
          return $ Map.fromList m

      -- | Names and types of arguments to the program entrypoint.
      arginfo :: Program -> Interp s [(String, Type)]
      arginfo p = do
          proc' <- lookupProcedure (main p)
          return $ LowCore.arguments proc'

      -- | Default values for SSM types.
      defaultValue :: Type -> SSMExp
      defaultValue TInt32 = Lit TInt32 $ LInt32 0
      defaultValue TInt64 = Lit TInt64 $ LInt64 0
      defaultValue TUInt64 = Lit TUInt64 $ LUInt64 0
      defaultValue TUInt8 = Lit TUInt8 $ LUInt8 0
      defaultValue TBool  = Lit TBool $ LBool False

      getReferences :: Program -> Map.Map String (Var s) -> [(String, Var s)]
      getReferences p m = case Map.lookup (main p) (funs p) of
          Just pr -> let refparams  = filter (\(_,t) -> isReference t) $ arguments pr
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
              (ref,_,_) <- lift' $ readSTRef v
              val <- lift' $ readSTRef ref
              tell [T.Result n val]

-- | Run the interpreter. This is quivalent to the `main` function generated by the code generator.
run :: Interp s ()
run = do tick
         st <- get
         tell [T.Instant (now st) (length (events st))]
         instant
  where
      -- | Runs one instant. It will advance model time to the next event time, and
      -- then call tick.
      instant :: Interp s ()
      instant = do
        st <- get
        if null (events st)
            then return ()
            else do now' <- nextEventTime
                    modify $ \st -> st { now     = now'
                                       , written = []
                                       }
                    tick
                    st <- get
                    tell [T.Instant (now st) (length (events st))]
                    instant
    
      tick :: Interp s ()
      tick = do
          performEvents
          mainloop

-- | This function will keep popping processes off the ready queue, running them and then
-- calling itself again until the ready queue is empty.
mainloop :: Interp s ()
mainloop = do
    st <- get
    if null (readyQueue st)
        then return ()
        else do p <- dequeue
                modify $ \st -> st { process = p }
                runProcess
                mainloop

-- | Run instructions of a process until the process should terminate (there are no
-- instructions left) or it should block (by evaluating a wait or fork instruction).
runProcess :: Interp s ()
runProcess = do
    i <- popInstruction
    p <- gets process
    case i of

        Nothing -> case parent p of
            Nothing -> return ()
            Just p' -> do
                par' <- lift' $ readSTRef p'
                if runningChildren par' == 1
                    then enqueue $ par' { runningChildren = 0 }
                    else lift' $ writeSTRef p' $ par' { runningChildren = runningChildren par' - 1 }

        Just stm -> case stm of
            NewRef n _ e    -> do
                newVar (getVarName n) e
                runProcess
            GetRef n t r    -> do
                v <- readRef (fst r)
                newVar (getVarName n) v
                runProcess
            SetRef r e      -> do
                writeRef (fst r) e
                runProcess
            SetLocal n t e2 -> do
                writeRef (getVarName n) e2
                runProcess
            If c thn els    -> do
                b <- getBool <$> eval c
                pushInstructions $ if b then thn else els
                runProcess
            While c bdy     -> do
                b <- getBool <$> eval c
                if b
                    then pushInstructions $ bdy ++ [stm]
                    else return ()
                runProcess
            Skip            -> runProcess
            After d r v     -> do
                ref  <- lookupRef (fst r)
                d'   <- getUInt64 <$> eval d
                v'   <- eval v
                now' <- gets now
                schedule_event $ Event (now' + d') ref v'
                -- TODO should not allow more than one event per variable to be outstanding
                --modify $ \st -> st { events = Event (now' + d') ref v' : events st }
                runProcess
            Changed n t r   -> do
                b <- wasWritten $ fst r
                newVar (getVarName n) b
                runProcess

            -- The statements below are blocking statements, so there is no nextInstruction
            -- statements after them. Wait blocks until either of the specified references
            -- received a new value, and fork blocks until all child processes have terminated.
            Wait refs       -> do
                wait refs
                mapM_ (sensitize . fst) refs
            Fork procs      -> do
                tell [T.Fork $ map fst procs]
                modify $ \st -> st { process = (process st) { runningChildren = length procs } }
                p <- gets process
                parent <- lift' $ newSTRef p

                pdeps <- pds (length procs)
                forM_ (zip procs pdeps) $ \(f,(prio, dep)) -> do
                    fork f prio dep parent

schedule_event :: Event s -> Interp s ()
schedule_event e = do
    st <- get
    let evs = events st

    when (now st > at e) $ error "bad after"

    if any ((==) e) evs
        then modify $ \st -> st { events = insert e (delete e evs) }
        else if numevents st == 8192
            then error "eventqueue full"
            else modify $ \st -> st { events    = insert e evs
                                    , numevents = numevents st + 1
                                    }
  where
      insert :: Event s -> [Event s] -> [Event s]
      insert e []       = [e]
      insert e1 (e2:es) =
          if at e1 < at e2
              then e1 : e2 : es
              else e2 : insert e1 es

lift' :: ST s a -> Interp s a
lift' = lift . lift

wait :: [Reference] -> Interp s ()
wait refs = do
    refs' <- mapM (lookupRef . fst) refs -- [Var s]
    modify $ \st -> st { process = (process st) { waitingOn = Just refs' } }

-- | Enqueue a new, forked process.
fork :: (String, [Either SSMExp Reference])  -- ^ Procedure to fork (name and arguments)
     -> Int                                  -- ^ Priority of the new process
     -> Int                                  -- ^ Depth of the new process
     -> STRef s (Proc s)                     -- ^ Reference to the parent process
     -> Interp s ()
fork (n,args) prio dep par = do
    p <- lookupProcedure n
    variables <- params $ (fst . unzip . LowCore.arguments) p
    enqueue $ Proc prio dep 0 (Just par) variables Nothing (body p)
  where
      -- | Return an initial variable storage for the new process. Expression arguments are turned into
      -- new STRefs while reference arguments are passed from the calling processes variable storage.
      params :: [String] -> Interp s (Map.Map String (Var s))
      params names = do
          st <- gets process
          m <- flip mapM (zip names args) $ \(n, a) ->
              case a of
                  Left e  -> do v <- eval e
                                lift' (newVar' v) >>= \v'  -> return (n, v')
                  Right r -> lookupRef (fst r)    >>= \ref -> return (n, ref)
          return $ Map.fromList m

writeRef :: String -> SSMExp -> Interp s ()
writeRef r e = do
    p <- gets process
    case Map.lookup r (variables p) of
        Just ref -> do v <- eval e
                       writeVar ref v
                       modify $ \st -> st { written = ref : written st }
        Nothing  -> error $ "interpreter error - can not find variable " ++ r

-- | Read a variable from the current processes environment.
readRef :: String -> Interp s SSMExp
readRef s = do
    r <- lookupRef s
    (vr,_,_) <- lift' $ readSTRef r
    lift' $ readSTRef vr

-- | Look up a variable in the currently running process variable store.
lookupRef :: String -> Interp s (Var s)
lookupRef r = do
    p <- gets process
    case Map.lookup r (variables p) of
      Just ref -> return ref
      Nothing  -> error $ "interpreter error - can not find variable " ++ r

-- | Simple lookup function that throws an error if the desired procedure does not exist in
-- the procedure storage.
lookupProcedure :: String -> Interp s Procedure
lookupProcedure n = do
    st <- get
    case Map.lookup n (procedures st) of
        Just p -> return p
        Nothing -> error $ "interpreter error - trying to fork non-existant procedure"

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

-- | Create a new variable with an initial value, and adds it to the current process's
-- variable storage. When a variable is created it is considered written to.
newVar :: String -> SSMExp -> Interp s ()
newVar n e = do
    v <- eval e
    ref <- lift' $ newVar' v
    p <- gets process
    modify $ \st -> st { written = ref : written st
                       , process = p { variables = Map.insert n ref (variables p) }
                       }

-- | Creates a new `Var s` with an initial value.
newVar' :: SSMExp -> ST s (Var s)
newVar' v = do
    val <- newSTRef v
    ref <- newSTRef (val, [], True)
    return ref

-- | Function returns True if variable was written in this instant, and otherwise False.
wasWritten :: String -> Interp s SSMExp
wasWritten r = do
    p <- gets process
    case Map.lookup r (variables p) of
        Just v -> do (_,_,b) <- lift' $ readSTRef v
                     return $ Lit TBool $ LBool $ b
        Nothing -> error $ "interpreter error - can not find variable " ++ r

writeVar :: Var s -> SSMExp -> Interp s ()
writeVar ref e = do
    p <- gets process
    writeVar_ ref e (priority p)

writeVar_ :: Var s -> SSMExp -> Int -> Interp s ()
writeVar_ ref e prio = do
    (variable,waits, _) <- lift' $ readSTRef ref
    lift' $ writeSTRef variable e -- actually update the variable value

    let (towait, keep) = partition (\p' -> prio < priority p') waits

    -- wake up and desensitize the processes
    mapM_ desensitize towait

    -- update the variable to be written to in this instant and give it knowledge of
    -- which processes are still waiting on it
    lift' $ writeSTRef ref (variable, keep, True)
  where
      desensitize :: Proc s -> Interp s ()
      desensitize p = do
          let variables = fromJust $ waitingOn p
          forM_ variables $ \r -> do
              (ref,procs,b) <- lift' $ readSTRef r
              lift' $ writeSTRef r (ref, delete p procs,b)
          enqueue $ p { waitingOn = Nothing}

-- | Make the procedure wait for writes to the variable
sensitize :: String -> Interp s ()
sensitize v = do
    p <- gets process
    r <- lookupRef v
    (ref,procs,b) <- lift' $ readSTRef r
    lift' $ writeSTRef r (ref, p:procs,b)

{- | Perform all the events scheduled for this instance, enqueueing those processes that
were waiting for one of these events to happen. -}
performEvents :: Interp s ()
performEvents = do
    es <- currentEvents
    mapM_ performEvent es
  where
      -- | Fetch the events at this instant in time, if any
      currentEvents :: Interp s [Event s]
      currentEvents = do
          st <- get
          let (current, future) = partition (\e -> at e == now st) (events st)
          put $ st { events = future
                   , numevents = numevents st - length current
                   }
          return current

      {- | Perform the update of a scheduled event and enqueue processes that were waiting for
      this event to happen. -}
      performEvent :: Event s -> Interp s ()
      performEvent e = do
          st <- get
          tell [T.Event (now st) (val e)]
          writeVar_ (ref e) (val e) (-1)
          modify $ \st -> st { written = ref e : written st}

-- | Fetch the process with the lowest priority from the ready queue
dequeue :: Interp s (Proc s)
dequeue = do
    st <- get
    case readyQueue st of
        [] -> error $ "interpreter error - dequeue called on empty readyqueue"
        (x:xs) -> do put $ st { readyQueue = xs}
                     return x

-- | Enqueue a process in the ready queue, ordered by its priority
enqueue :: Proc s -> Interp s ()
enqueue p = modify $ \st -> st { readyQueue = insert p (readyQueue st)}
  where
      insert :: Proc s -> [Proc s] -> [Proc s]
      insert p []       = [p]
      insert p1 (p2:ps) = if priority p1 < priority p2
                            then p1 : p2 : ps
                            else p2 : insert p1 ps

-- | Inspects the eventqueue and returns the next event time.
nextEventTime :: Interp s Word64
nextEventTime = do
    evs <- gets events
    return $ foldl min maxBound (map at evs)

-- | Return the next instruction of the current process if one exists.
popInstruction :: Interp s (Maybe Stm)
popInstruction = do
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

eval :: SSMExp -> Interp s SSMExp
eval e = do
    p <- gets process
    case e of
        Var _ n -> case Map.lookup n (variables p) of
            Just r -> do v <- lift $ lift $ (readSTRef . \(x,_,_) -> x) =<< readSTRef r
                         eval v
            Nothing -> error $ "interpreter error - variable " ++ n ++ " not found in current process"
        Lit _ l -> return e
        UOp _ e Neg -> do
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
                OMinus -> return $ LowInterpreter.subtract i1 i2
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

getInt :: SSMExp -> Int32
getInt (Lit _ (LInt32 i)) = i
getInt e                = error $ "not an integer: " ++ show e

getInt64 :: SSMExp -> Int64
getInt64 (Lit _ (LInt64 i)) = i
getInt64 e                  = error $ "not an integer: " ++ show e

getUInt64 :: SSMExp -> Word64
getUInt64 (Lit _ (LUInt64 i)) = i
getUInt64 e                  = error $ "not an integer: " ++ show e

getBool :: SSMExp -> Bool
getBool (Lit _ (LBool b)) = b
getBool e                 = error $ "not a boolean: " ++ show e

-- | Infix operator for applying a unlifted argument to a lifted function.
-- Surprised that I did not find this anywhere.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
fa <#> b = fa <*> pure b
infixl 4 <#>
