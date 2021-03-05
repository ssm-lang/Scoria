module Interpreter where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.State
import Data.List
import GHC.Float

import Core

data Event = Event
    { at  :: Int           -- ^ The time when this event occurs
    , ref :: IORef SSMExp  -- ^ The reference variable that gets the new value
    , val :: SSMExp        -- ^ The value this reference will assume at time at
    }

data Proc = Proc
    { priority        :: Int                            -- ^ priority of the process
    , depth           :: Int                            -- ^ The depth, which helps give priorities to children
    , runningChildren :: Int                            -- ^ Number of non-terminated child processes
    , parent          :: Maybe (IORef Proc)             -- ^ Parent of this process, Nothing in the case of main
    , references      :: Map.Map String (IORef SSMExp)  -- ^ Reference variables and their values
    , variables       :: Map.Map String (IORef SSMExp)  -- ^ Local variables and their values
    , waitingOn       :: Maybe [IORef SSMExp]           -- ^ Variables this process is waiting for, if any
    , continuation    :: SSM ()                         -- ^ The work left to do for this process
    }

data St = St
    { now        :: Int                           -- ^ Current time
    , arguments  :: Map.Map String (IORef SSMExp) -- ^ Arguments to main
    , events     :: [Event]                       -- ^ Outstanding events
    , readyQueue :: [Proc]                        -- ^ Processes ready to run, should be a priority queue
    , waiting    :: [Proc]                        -- ^ Processes that are waiting on variables
    , written    :: [IORef SSMExp]                -- ^ References that were written in this instance
    }

type Interp a = StateT St IO a

interpret :: SSM () -> [(String, SSMExp)] -> IO ()
interpret ssm args = do
    putStrLn "beginning interpretation --"
    -- Create references for the program arguments and create the state
    args' <- mkArgs args
    let p = Proc 1024 10 0 Nothing Map.empty Map.empty Nothing ssm
    let state = st (Map.fromList args') p
    putStrLn "done setting up program arguments --"

    -- Run the actual program
    putStrLn "running the program --"
    runStateT mainloop state
    
    -- Print the state of the references after program termination
    putStrLn "program terminated -- state of input arguments are:"
    forM_ args' $ \(n,r) -> do
        v <- readIORef r
        putStrLn $ n ++ ": " ++ show v
  where
      st :: Map.Map String (IORef SSMExp) -> Proc -> St
      st args p = St (-1) args [] [p] [] []

      mkArgs :: [(String, SSMExp)] -> IO [(String, IORef SSMExp)]
      mkArgs args = mapM (\(n,v) -> newIORef v >>= (\r -> return (n, r))) args

{- | Mainloop of a program. If there is work left to do it will increment the current
time, perform the outstanding events for this time and then run the scheduler. -}
mainloop :: Interp ()
mainloop = do
    st <- get
    if null (events st) && null (readyQueue st)
        then return ()
        else do liftIO $ putStrLn $ "TIME: " ++ show (now st + 1)
                modify $ \st -> st { now = now st + 1
                                   , written = []
                                   }
                performEvents
                runProcesses
                mainloop

{- | Dequeues the process with lowest priority and runs it until the ready queue
becomes empty. It is worth mentioning and running a process might enqueue additional
processes on the ready queue. -}
runProcesses :: Interp ()
runProcesses = do
    mp <- dequeue
    case mp of
        Just p  -> runProcess p >> runProcesses
        Nothing -> return ()

-- this is very non exhaustive, fix later
eval :: Proc -> SSMExp -> Interp SSMExp
eval p e = do
    case e of
        Var n -> case Map.lookup n (variables p) of
            Just r -> do v <- liftIO $ readIORef r
                         eval p v
            Nothing -> error $ "interpreter error - variable " ++ n ++ " not found in current process"
        Lit l -> return e
        UOp e Neg -> do
            e' <- eval p e
            let Lit (LInt i) = e'
            return $ Lit $ LInt (-i)
        BOp e1 e2 op -> do
            se1 <- eval p e1
            se2 <- eval p e2
            case op of
                OPlus  -> let (Lit (LInt i1)) = se1
                              (Lit (LInt i2)) = se2
                        in return $ Lit $ LInt $ i1 + i2
                OMinus -> let (Lit (LInt i1)) = se1
                              (Lit (LInt i2)) = se2
                        in return $ Lit $ LInt $ i1 - i2
                OTimes -> let (Lit (LInt i1)) = se1
                              (Lit (LInt i2)) = se2
                        in return $ Lit $ LInt $ i1 * i2
                OLT    -> let (Lit (LInt i1)) = se1
                              (Lit (LInt i2)) = se2
                        in return $ Lit $ LBool $ i1 < i2
                OEQ    -> let (Lit (LInt i1)) = se1
                              (Lit (LInt i2)) = se2
                        in return $ Lit $ LBool $ i1 == i2

runProcess :: Proc -> Interp ()
runProcess p = case continuation p of
    Return x          -> return ()
    NewRef n e k      -> do
        --liftIO $ putStrLn $ "newref"
        r <- liftIO $ newIORef e
        runProcess $ p { references   = Map.insert n r (references p)
                       , continuation = k n
                       }
    SetRef r e k -> do
        --liftIO $ putStrLn $ "setref"
        writeRef p r e
        runProcess $ p { continuation = k ()}
    SetLocal (Var r) v k -> do
        --liftIO $ putStrLn "setlocal"
        writeRef p r v
        runProcess $ p { continuation = k ()}
    SetLocal e v k -> error $ "interpreter error - can not assign value to expression: " ++ show e
    GetRef r k -> do
        --liftIO $ putStrLn $ "getref"
        ior <- lookupRef r p
        e <- liftIO $ readIORef ior
        runProcess $ p { continuation = k e}
    If c thn (Just els) k -> do
        --liftIO $ putStrLn $ "if"
        b <- eval p c
        case b of
          Lit (LBool True)  -> runProcess $ p { continuation = thn >> k ()}
          Lit (LBool False) -> runProcess $ p { continuation = els >> k ()}
    If c thn Nothing k -> do
        --liftIO $ putStrLn $ "if"
        b <- eval p c
        case b of
          Lit (LBool True)  -> runProcess $ p { continuation = thn >> k ()}
          Lit (LBool False) -> runProcess $ p { continuation = k ()}
    While c bdy k -> do
        --liftIO $ putStrLn $ "while"
        b <- eval p c
        case b of
          Lit (LBool True)  -> runProcess $ p { continuation = bdy >> continuation p}
          Lit (LBool False) -> runProcess $ p { continuation = k ()}
    After e r v k -> do
        --liftIO $ putStrLn $ "after"
        i <- eval p e
        case i of
          Lit (LInt num) -> do
              ref <- lookupRef r p
              t   <- gets now
              v'  <- eval p v
              modify $ \st -> st { events = Event (t + num) ref v' : events st }
              runProcess $ p { continuation = k ()}
          _ -> error $ "interpreter error - not a number " ++ show i
    Changed r k       -> do
        --liftIO $ putStrLn $ "changed"
        st <- get
        ref <- lookupRef r p
        if ref `elem` written st
            then runProcess $ p { continuation = k (Lit (LBool True))}
            else runProcess $ p { continuation = k (Lit (LBool False))}
    Wait vars k       -> do
        --liftIO $ putStrLn $ "wait"
        refs <- mapM (`lookupRef` p) vars
        wait $ p { waitingOn = Just refs
                 , continuation = k ()
                 }
    Fork procs k      -> do
        --liftIO $ putStrLn $ "fork"
        let numchild = length procs
        let d'       = depth p - integerLogBase 2 (toInteger $ depth p)
        let priodeps = [ (priority p + p'*(2^d'), d') | p' <- [0 .. numchild-1]]
        let p'       = p { runningChildren = numchild
                         , continuation = k ()
                         }
        par         <- liftIO $ newIORef p'
        forM_ (zip procs priodeps) $ \(cont, (prio, dep)) -> do
            enqueue $ Proc prio dep 0 (Just par) Map.empty Map.empty Nothing cont
    Procedure n f k   -> do
        --liftIO $ putStrLn $ "procedure"
        runProcess $ p { continuation = k ()}
    Argument n m a k  -> do
        --liftIO $ putStrLn $ "argument"
        case a of
            Left e  -> do
                v <- case parent p of
                    Just par -> do p' <- liftIO $ readIORef par
                                   eval p' e
                    Nothing  -> return e
                ref <- liftIO $ newIORef v
                runProcess $ p { variables = Map.insert m ref (variables p)
                               , continuation = k ()
                               }
            Right r -> case parent p of
                Just par -> do
                    p'  <- liftIO $ readIORef par
                    ref <- lookupRef r p'
                    runProcess $ p { references = Map.insert m ref (references p)
                                   , continuation = k ()}
                Nothing  -> do
                    st <- get
                    case Map.lookup r (arguments st) of
                        Just ref -> do
                            runProcess $ p { references = Map.insert m ref (references p)
                                           , continuation = k ()
                                           }
                        Nothing  -> error $ "interpreter error - unknown reference " ++ r
    Result n r k -> do
        --liftIO $ putStrLn $ "result"
        case parent p of
            Just par -> do
                p' <- liftIO $ readIORef par
                if runningChildren p' == 1
                    then enqueue $ p' { runningChildren = 0}
                    else liftIO $ writeIORef par $ p' { runningChildren = runningChildren p' - 1}
            Nothing  -> return ()
        runProcess $ p { continuation = k ()} -- this will probably just be one more return
  where
      writeRef :: Proc -> Reference -> SSMExp -> Interp ()
      writeRef p r e = do
          case Map.lookup r (references p) of
              Just ref -> do v <- eval p e
                             liftIO $ writeIORef ref v
                             modify $ \st -> st { written = ref : written st }
              Nothing  -> case Map.lookup r (variables p) of
                  Just ref -> do v <- eval p e
                                 liftIO $ writeIORef ref v
                                 modify $ \st -> st { written = ref : written st }
                  Nothing  -> error $ "interpreter error - not not find reference " ++ r


      wait :: Proc -> Interp ()
      wait p = modify $ \st -> st { waiting = p : waiting st}

lookupRef :: Reference -> Proc -> Interp (IORef SSMExp)
lookupRef r p = case Map.lookup r (references p) of
    Just ref -> return ref
    Nothing  -> error $ "interpreter error - can not find reference " ++ r

{- | Perform all the events scheduled for this isntance, enqueueing those processes that
were waiting for one of these events to happen. -}
performEvents :: Interp ()
performEvents = do
    es <- currentEvents
    mapM_ performEvent es
  where
      -- | Fetch the events at this instant in time, if any
      currentEvents :: Interp [Event]
      currentEvents = do
          st <- get
          let (current, future) = partition (\e -> at e == now st) (events st)
          put $ st { events = future}
          return current

      {- | Perform the update of a scheduled event and enqueue processes that were waiting for
      this event to happen. -}
      performEvent :: Event -> Interp ()
      performEvent e = do
          liftIO $ writeIORef (ref e) (val e)
          modify $ \st -> st { written = ref e : written st}
          processes <- waitingProcesses
          mapM_ enqueueWaiting processes
        where
            -- | Enqueue a process after making sure it is not waiting on any reference anymore
            enqueueWaiting :: Proc -> Interp ()
            enqueueWaiting p = do
                enqueue $ p { waitingOn = Nothing}

            -- | Processes that are waiting for this event
            waitingProcesses :: Interp [Proc]
            waitingProcesses = do
                st <- get
                let (w, nw) = partition pred (waiting st)
                put $ st { waiting = nw}
                return w
                where
                    pred :: Proc -> Bool
                    pred p = case waitingOn p of
                        Just vars -> ref e `elem` vars
                        Nothing   -> False -- should really be an error

-- | Fetch the process with the lowest priority from the ready queue
dequeue :: Interp (Maybe Proc)
dequeue = do
    st <- get
    case readyQueue st of
        [] -> return Nothing --error "interpreter error -- ready queue empty"
        (x:xs) -> do put $ st { readyQueue = xs}
                     return (Just x)

-- | Enqueue a process in the ready queue, ordered by its priority
enqueue :: Proc -> Interp ()
enqueue p = modify $ \st -> st { readyQueue = insert p (readyQueue st)}
  where
      insert :: Proc -> [Proc] -> [Proc]
      insert p [] = [p]
      insert p1 (p2:ps) = if priority p1 < priority p2
                            then p1 : p2 : ps
                            else p2 : insert p1 ps