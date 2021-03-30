module Interpreter where

import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.List
import Data.Maybe
import GHC.Float

import Core
import qualified Trace as T

type Var s = STRef s (STRef s SSMExp, [Proc s])

data Event s = Event
    { at  :: Int     -- ^ The time when this event occurs
    , ref :: Var s   -- ^ The reference variable that gets the new value
    , val :: SSMExp  -- ^ The value this reference will assume at time at
    } deriving Eq

data Proc s = Proc
    { priority        :: Int                       -- ^ priority of the process
    , depth           :: Int                       -- ^ The depth, which helps give priorities to children
    , runningChildren :: Int                       -- ^ Number of non-terminated child processes
    , parent          :: Maybe (STRef s (Proc s))  -- ^ Parent of this process, Nothing in the case of main
    , references      :: Map.Map String (Var s)    -- ^ Reference variables and their values
    , variables       :: Map.Map String (Var s)    -- ^ Local variables and their values
    , waitingOn       :: Maybe [Var s]             -- ^ Variables this process is waiting for, if any
    , continuation    :: SSM ()                    -- ^ The work left to do for this process
    }

instance Eq (Proc s) where
    p1 == p2 =  priority p1        == priority p2
             && depth p1           == depth p2
             && runningChildren p1 == runningChildren p2
             && parent p1          == parent p2
             && references p1      == references p2
             && variables p1       == variables p2
             && waitingOn p1       == waitingOn p2

data St s = St
    { now        :: Int                     -- ^ Current time
    , arguments  :: Map.Map String (Var s)  -- ^ Arguments to main
    , events     :: [Event s]               -- ^ Outstanding events
    , readyQueue :: [Proc s]                -- ^ Processes ready to run, should be a priority queue
    , written    :: [Var s]                 -- ^ References that were written in this instance
    , counter    :: Int                     -- ^ Counter for fresh name supply
    } deriving Eq

type Interp s a = StateT (St s) (WriterT T.Output (ST s)) a


interpret :: SSM () -> T.Output
interpret ssm = snd $ runST (interpret' ssm)

interpret' :: SSM () -> ST s ((), T.Output)
interpret' ssm = do
    args     <- mkArgs ssm
    let p     = Proc 1024 10 0 Nothing Map.empty Map.empty Nothing ssm
    let state = St 0 (Map.fromList args) [] [p] [] 0
    runWriterT (evalStateT mainloop' state)
  where
      mkArgs :: SSM () -> ST s [(String, Var s)]
      mkArgs (Procedure _ k)                = mkArgs $ k ()
      mkArgs (Argument _ n (Left e) k)      = mkArgs $ k ()
      mkArgs (Argument _ n (Right (r,t)) k) = do
          ref <- newSTRef (defaultVal (dereference t))
          variable <- newSTRef (ref, [])
          rest <- mkArgs $ k ()
          return $ (n,variable) : rest
      mkArgs _                              = return []

      defaultVal :: Type -> SSMExp
      defaultVal TInt  = Lit TInt (LInt 0)
      defaultVal TBool = Lit TBool (LBool False)
      defaultVal t     = error $ "not a simple type: " ++ show t

      mainloop' :: Interp s ()
      mainloop' = do
          runProcesses
          st <- get
          tell [T.Instant (now st) (length (events st))]
          go
        where
            go :: Interp s ()
            go = do 
                next_time <- next_event_time
                case next_time of
                    Just next -> do modify $ \st -> st { now = next, written = []}
                                    performEvents
                                    runProcesses
                                    st <- get
                                    tell [T.Instant (now st) (length (events st))]
                                    go
                    Nothing   -> return ()

            next_event_time :: Interp s (Maybe Int)
            next_event_time = do
                evs <- gets events
                if null evs
                    then return Nothing
                    else let next = foldl min (at (head evs)) $ tail $ map at evs
                        in return $ Just next

{- | Dequeues the process with lowest priority and runs it until the ready queue
becomes empty. It is worth mentioning and running a process might enqueue additional
processes on the ready queue. -}
runProcesses :: Interp s ()
runProcesses = do
    mp <- dequeue
    case mp of
        Just p  -> runProcess p >> runProcesses
        Nothing -> return ()


eval :: Proc s -> SSMExp -> Interp s SSMExp
eval p e = do
    case e of
        Var _ n -> case Map.lookup n (variables p) of
            Just r -> do v <- lift $ lift $ (readSTRef . fst) =<< readSTRef r
                         eval p v
            Nothing -> error $ "interpreter error - variable " ++ n ++ " not found in current process"
        Lit _ l -> return e
        UOp _ e Neg -> do
            e' <- eval p e
            let Lit _ (LInt i) = e'
            return $ Lit TInt $ LInt (-i)
        BOp TInt e1 e2 op -> do
            i1 <- getInt <$> eval p e1
            i2 <- getInt <$> eval p e2
            case op of
                OPlus  -> return $ Lit TInt  $ LInt  (i1 + i2)
                OMinus -> return $ Lit TInt  $ LInt  (i1 - i2)
                OTimes -> return $ Lit TInt  $ LInt  (i1 * i2)
        BOp TBool e1 e2 op -> do
            l1 <- eval p e1
            l2 <- eval p e2
            case op of
                OLT -> let i1 = getInt l1
                           i2 = getInt l2
                       in return $ Lit TBool $ LBool (i1 < i2)
                OEQ -> case (expType e1) of
                    TInt  -> let i1 = getInt l1
                                 i2 = getInt l2
                             in return $ Lit TBool $ LBool (i1 == i2)
                    TBool -> let b1 = getBool l1
                                 b2 = getBool l2
                             in return $ Lit TBool $ LBool (b1 == b2)
  where
      getInt :: SSMExp -> Int
      getInt (Lit _ (LInt i)) = i
      getInt e                = error $ "not an integer: " ++ show e

      getBool :: SSMExp -> Bool
      getBool (Lit _ (LBool b)) = b
      getBool e                 = error $ "not a boolean: " ++ show e

newVar :: SSMExp -> Interp s (Var s)
newVar e = do
    r <- lift $ lift $ newSTRef e
    ref <- lift $ lift $ newSTRef (r, [])
    return ref

runProcess :: Proc s -> Interp s ()
runProcess p = case continuation p of
    Return x          -> return ()
    NewRef ba e k      -> do
        name <- case ba of
            Just (_,n) -> return n
            Nothing    -> fresh
        r <- newVar e
        runProcess $ p { references   = Map.insert name r (references p)
                       , continuation = k (name, mkReference (expType e))
                       }
    SetRef r e k -> do
        writeRef p r e
        runProcess $ p { continuation = k ()}
    SetLocal (Var t r) v k -> do
        writeRef p (r,t) v
        runProcess $ p { continuation = k ()}
    SetLocal e v k -> error $ "interpreter error - can not assign value to expression: " ++ show e
    GetRef r s k -> do
        ior <- lookupRef r p
        (r,_) <- lift $ lift $ readSTRef ior
        e <- lift $ lift $ readSTRef r
        runProcess $ p { continuation = k e}
    If c thn (Just els) k -> do
        b <- eval p c
        case b of
          Lit _ (LBool True)  -> runProcess $ p { continuation = thn >> k ()}
          Lit _ (LBool False) -> runProcess $ p { continuation = els >> k ()}
    If c thn Nothing k -> do
        b <- eval p c
        case b of
          Lit _ (LBool True)  -> runProcess $ p { continuation = thn >> k ()}
          Lit _ (LBool False) -> runProcess $ p { continuation = k ()}
    While c bdy k -> do
        b <- eval p c
        case b of
          Lit _ (LBool True)  -> runProcess $ p { continuation = bdy >> continuation p}
          Lit _ (LBool False) -> runProcess $ p { continuation = k ()}
    After e r v k -> do
        i <- eval p e
        case i of
          Lit _ (LInt num) -> do
              ref <- lookupRef r p
              t   <- gets now
              v'  <- eval p v
              modify $ \st -> st { events = Event (t + num) ref v' : events st }
              runProcess $ p { continuation = k ()}
          _ -> error $ "interpreter error - not a number " ++ show i
    Changed r s k     -> do
        st <- get
        ref <- lookupRef r p
        if ref `elem` written st
            then runProcess $ p { continuation = k (Lit TBool (LBool True))}
            else runProcess $ p { continuation = k (Lit TBool (LBool False))}
    Wait vars k       -> do
        refs <- mapM (`lookupRef` p) vars
        let p' = p { waitingOn    = Just refs
                   , continuation = k ()
                   }
        mapM_ (sensitize p') refs
    Fork procs k      -> do
        let numchild = length procs
        let d'       = depth p - integerLogBase 2 (toInteger $ depth p)
        let priodeps = [ (priority p + p'*(2^d'), d') | p' <- [0 .. numchild-1]]
        let p'       = p { runningChildren = numchild
                         , continuation = k ()
                         }
        par         <- lift $ lift $ newSTRef p'
        forM_ (zip procs priodeps) $ \(cont, (prio, dep)) -> do
            enqueue $ Proc prio dep 0 (Just par) Map.empty Map.empty Nothing cont
    Procedure n k     -> do
        runProcess $ p { continuation = k ()}
    Argument n m a k  -> do
        case a of
            Left e  -> do
                v <- case parent p of
                    Just par -> do p' <- lift $ lift $ readSTRef par
                                   eval p' e
                    Nothing  -> return e
                ref <- newVar v
                runProcess $ p { variables = Map.insert m ref (variables p)
                               , continuation = k ()
                               }
            Right r -> case parent p of
                Just par -> do
                    p'  <- lift $ lift $ readSTRef par
                    ref <- lookupRef r p'
                    runProcess $ p { references = Map.insert m ref (references p)
                                   , continuation = k ()}
                Nothing  -> do
                    st <- get
                    case Map.lookup m (arguments st) of
                        Just ref -> do
                            runProcess $ p { references = Map.insert m ref (references p)
                                           , continuation = k ()
                                           }
                        Nothing  -> error $ "interpreter error - unknown reference " ++ fst r
    Result n r k -> do
        case parent p of
            Just par -> do
                p' <- lift $ lift $ readSTRef par
                if runningChildren p' == 1
                    then enqueue $ p' { runningChildren = 0}
                    else lift $ lift $ writeSTRef par $ p' { runningChildren = runningChildren p' - 1}
            Nothing  -> return ()
        runProcess $ p { continuation = k ()} -- this will probably just be one more return
  where
      writeRef :: Proc s -> Reference -> SSMExp -> Interp s ()
      writeRef p (r,_) e = do
          case Map.lookup r (references p) of
              Just ref -> do v <- eval p e
                             writeVar ref v
                             modify $ \st -> st { written = ref : written st }
              Nothing  -> case Map.lookup r (variables p) of
                  Just ref -> do v <- eval p e
                                 writeVar ref v
                                 modify $ \st -> st { written = ref : written st }
                  Nothing  -> error $ "interpreter error - not not find reference " ++ r

      fresh :: Interp s String
      fresh = do
          st <- gets counter
          modify $ \st -> st { counter = counter st + 1}
          return $ "v" ++ show st

lookupRef :: Reference -> Proc s -> Interp s (Var s)
lookupRef (r,_) p = case Map.lookup r (references p) of
    Just ref -> return ref
    Nothing  -> error $ "interpreter error - can not find reference " ++ r

writeVar :: Var s -> SSMExp -> Interp s ()
writeVar ref e = do
    (variable,waits) <- lift $ lift $ readSTRef ref
    lift $ lift $ writeSTRef variable e
    mapM_ desensitize waits
    lift $ lift $ writeSTRef ref (variable, [])
  where
      desensitize :: Proc s -> Interp s ()
      desensitize p = do
          let variables = fromJust $ waitingOn p
          forM_ variables $ \r -> do
              (ref,procs) <- lift $ lift $ readSTRef r
              lift $ lift $ writeSTRef r (ref, delete p procs)
          enqueue $ p { waitingOn = Nothing}

-- | Make the procedure wait for writes to the variable
sensitize :: Proc s -> Var s -> Interp s ()
sensitize p v = do
    (ref,procs) <- lift $ lift $ readSTRef v
    lift $ lift $ writeSTRef v (ref, p:procs)

{- | Perform all the events scheduled for this isntance, enqueueing those processes that
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
          put $ st { events = future}
          return current

      {- | Perform the update of a scheduled event and enqueue processes that were waiting for
      this event to happen. -}
      performEvent :: Event s -> Interp s ()
      performEvent e = do
          st <- get
          tell [T.Event (now st) (val e)]
          writeVar (ref e) (val e)
          modify $ \st -> st { written = ref e : written st}

-- | Fetch the process with the lowest priority from the ready queue
dequeue :: Interp s (Maybe (Proc s))
dequeue = do
    st <- get
    case readyQueue st of
        [] -> return Nothing --error "interpreter error -- ready queue empty"
        (x:xs) -> do put $ st { readyQueue = xs}
                     return (Just x)

-- | Enqueue a process in the ready queue, ordered by its priority
enqueue :: Proc s -> Interp s ()
enqueue p = modify $ \st -> st { readyQueue = insert p (readyQueue st)}
  where
      insert :: Proc s -> [Proc s] -> [Proc s]
      insert p [] = [p]
      insert p1 (p2:ps) = if priority p1 < priority p2
                            then p1 : p2 : ps
                            else p2 : insert p1 ps
