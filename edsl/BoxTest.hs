{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module BoxTest where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.List
import Data.IORef

import GHC.Float

import qualified Data.Map as Map

--import Language.C.Data.Ident
--import Language.C.Data.Node
--import qualified Language.C.Syntax as C


type Reference = String

data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a

    -- | Variable/Stream operations
    NewRef   :: String -> SSMExp -> (Reference -> SSM b) -> SSM b
    SetRef   :: Reference -> SSMExp -> (() -> SSM b) -> SSM b
    GetRef   :: Reference -> (SSMExp -> SSM b) -> SSM b
    SetLocal :: SSMExp -> SSMExp -> (() -> SSM b) -> SSM b
    
    -- | Control operations
    If     :: SSMExp -> SSM () -> Maybe (SSM ()) -> (() -> SSM b) -> SSM b
    While  :: SSMExp -> SSM () -> (() -> SSM b) -> SSM b
    
    -- | SSM specific operations
    After   :: SSMExp -> Reference -> SSMExp -> (() -> SSM b) -> SSM b
    Changed :: Reference -> (SSMExp -> SSM b) -> SSM b
    Wait    :: [Reference] -> (() -> SSM b) -> SSM b
    Fork    :: [SSM ()] -> (() -> SSM b) -> SSM b

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> (() -> SSM c) -> SSM c
    Argument  :: String -> String -> Either SSMExp Reference -> (() -> SSM b) -> SSM b
    Result    :: (Show a, Res a) => String -> a -> (() -> SSM b) -> SSM b
--    Tag :: String -> (() -> SSM b) -> SSM b


instance Monad SSM where
    return = Return

    Return x          >>= fa = fa x
    NewRef n e k      >>= fa = NewRef n e      (\x -> k x >>= fa)
    SetRef r e k      >>= fa = SetRef r e      (\x -> k x >>= fa)
    SetLocal e v k    >>= fa = SetLocal e v    (\x -> k x >>= fa)
    GetRef r k        >>= fa = GetRef r        (\x -> k x >>= fa)
    If c thn els k    >>= fa = If c thn els    (\x -> k x >>= fa)
    While c bdy k     >>= fa = While c bdy     (\x -> k x >>= fa)
    After e r v k     >>= fa = After e r v     (\x -> k x >>= fa)
    Changed r k       >>= fa = Changed r       (\x -> k x >>= fa)
    Wait vars k       >>= fa = Wait vars       (\x -> k x >>= fa)
    Fork procs k      >>= fa = Fork procs      (\x -> k x >>= fa)
    Procedure n f k   >>= fa = Procedure n f   (\x -> k x >>= fa)
    Argument n m a k  >>= fa = Argument n m a  (\x -> k x >>= fa)
    Result n r k      >>= fa = Result n r      (\x -> k x >>= fa)

instance Functor SSM where
    fmap f ma = do
        a <- ma
        return $ f a

-- Forces sequential composition, while the interface makes no such demand.
instance Applicative SSM where
    pure = return
    fa <*> ma = do
        f <- fa
        m <- ma
        return $ f m

-- | SSM expressions
data SSMExp = Var String               -- ^ Variables
            | Lit SSMLit               -- ^ Literals
            | UOp SSMExp UnaryOp       -- ^ Unary operators
            | BOp SSMExp SSMExp BinOp  -- ^ Binary operators
  deriving (Eq)

instance Show SSMExp where
    show (Var n)        = n
    show (Lit l)        = show l
    show (UOp e op)     = show op ++ " " ++ show e
    show (BOp e1 e2 op) = show e1 ++ " " ++ show op ++ " " ++ show e2 -- does not take precedence into account

-- | SSM literals
data SSMLit = LInt Int    -- ^ Integer literals
            | LBool Bool  -- ^ Boolean literals
  deriving (Eq)

instance Show SSMLit where
    show (LInt i)  = show i
    show (LBool b) = show b

{-- | SSM unary operators -}
data UnaryOp = Neg  -- ^ negation
  deriving (Eq)

instance Show UnaryOp where
    show Neg = "neg"

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp = OPlus   -- ^ addition
           | OMinus  -- ^ subtraction
           | OTimes  -- ^ multiplication
           | OLT     -- ^ less-than
           | OEQ     -- ^ eq
  deriving (Eq)

instance Show BinOp where
    show OPlus  = "+"
    show OMinus = "-"
    show OTimes = "*"
    show OLT    = "<"
    show OEQ    = "=="

-- | Arguments to our functions
class Arg a where
    arg :: String -> [String] -> a -> SSM (a, [String])

instance (Arg a, Arg b) => Arg (a,b) where
    arg name names (x,y) = do
        (x', names')  <- arg name names  x
        (y', names'') <- arg name names' y
        return ((x',y'), names'')

class Res b where
    result :: String -> b -> SSM b

class Box b where
    box :: Arg a => String -> [String] -> (a -> b) -> (a -> b)

instance (Arg b, Box c) => Box (b -> c) where
    box name xs f = curry (box name xs (uncurry f))

instance Res b => Box (SSM b) where
    box name xs f = \x -> do
        Procedure name f return
        (x',_) <- arg name xs x
        y'     <- f x'
        result name y'

type Ref a = Reference -- references that are shared, (variable name, ref to value)
type Exp a = SSMExp                 -- expressions

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name (x:xs) b = Argument name x (Left b) return >> return (Var x, xs)

instance Arg (Ref a) where
    arg name (x:xs) e = Argument name x (Right e) return >> return (x, xs)

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () return >> return ()

class Assignable a where
    (<~) :: a -> SSMExp -> SSM ()

instance Assignable (Exp a) where
    (Var s) <~ e = SetLocal (Var s) e return
    e <~ _       = error $ "can not assign a value to expression: " ++ show e

instance Assignable (Ref a) where
    r <~ e = SetRef r e return

(+.) :: Exp a -> Exp a -> Exp a
e1 +. e2  = BOp e1 e2 OPlus

(-.) :: Exp a -> Exp a -> Exp a
e1 -. e2  = BOp e1 e2 OMinus

(*.) :: Exp a -> Exp a -> Exp a
e1 *. e2  = BOp e1 e2 OTimes

(<.) :: Exp a -> Exp a -> Exp Bool
e1 <. e2  = BOp e1 e2 OLT

(==.) :: Exp a -> Exp a -> Exp Bool
e1 ==. e2 = BOp e1 e2 OEQ

neg :: Exp a -> Exp a
neg e = UOp e Neg

int :: Int -> Exp Int
int i = Lit $ LInt i

deref :: Ref a -> SSM (Exp a)
deref r = GetRef r return

var :: String -> Exp a -> SSM (Ref a)
var n e = NewRef n e return

wait :: [Ref a] -> SSM ()
wait r = Wait r return

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after e r v = After e r v return

fork :: [SSM ()] -> SSM ()
fork procs = Fork procs return

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed r = Changed r return

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' c thn els = If c thn els return

while' :: Exp Bool -> SSM () -> SSM ()
while' c bdy = While c bdy return

{- when we use Haskell as a host language we can define nice helper functions
like these without having to define a complete separate procedure. -}
waitall :: [Ref a] -> SSM ()
waitall refs = fork $ map mywait refs

{- An operator like this would be nice, if we want to use a reference in the
expression that computes the boolean. Will probably need to add a new constructor
WhileM :: SSM SSMExp -> SSM () -> .... and then use a typeclass to be able to provide
a single function while that works for both cases. -}
whileSSM :: SSM (Exp Bool) -> SSM () -> SSM ()
whileSSM sc sbdy = undefined

{-mysum r1 r2 r = do
    fork (mywait r1) (mywait r2)
    v1 <- deref r1
    v2 <- deref r2
    after 1 r (r1 + r2)-}

mywait :: Ref Int -> SSM ()
mywait = box "mywait" ["r"] $ \r -> do
    wait [r]

mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" ["r1", "r2", "r"] $ \r1 r2 r -> do
    fork [ mywait r1
         , mywait r2
         ]
    v1 <- deref r1
    v2 <- deref r2
    after (int 1) r (v1 +. v2)

myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" ["n", "r"] $ \n r -> do
    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. int 2)
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) r1
                        , myfib (n -. int 2) r2
                        , mysum r1 r2 r
                        ]))

test :: Ref Int -> SSM ()
test = box "test" ["r"] $ \r -> do
    after (int 5) r (int 5)
    fork [child r]
  where
      child :: Ref Int -> SSM ()
      child = box "child" ["r"] $ \r -> do
          wait [r]
          v <- deref r
          after (int 5) r (v +. int 5)

test2 :: Ref Int -> SSM ()
test2 = box "test2" ["r"] $ \r -> do
    after (int 5) r (int 5)
    fork [child r]
  where
      child :: Ref Int -> SSM ()
      child = box "child" ["r"] $ \r -> do
          --wait [r] -- uncommenting this line changes the result accordingly
          b <- changed r
          if' b (do v <- deref r
                    after (int 5) r (v +. int 5))
                (Just (do v <- deref r
                          after (int 5) r (v +. int 20)))
          v <- deref r
          after (int 5) r (v +. int 5)

test3 :: Ref Int -> SSM ()
test3 = box "test3" ["r"] $ \r -> do
    v <- deref r
    while' (v <. int 10) $ do -- TODO: can not do this yet (wait until r becomes <. 10)
      wait [r]
      fork [child r, child r, child r]
  where
      child :: Ref Int -> SSM ()
      child = box "child" ["r"] $ \r -> do
          v <- deref r
          after (int 2) r (v +. int 1)

test4 :: Exp Int -> Ref Int -> SSM ()
test4 = box "test4" ["n", "r"] $ \n r -> do
    while' (n <. int 10) $ do
        n <~ (n +. int 1)
    r <~ n

type PPSt = ( Int       -- counter to generate fresh names
            , [String]  -- names of already generated functions
            , [String]  -- generated functions
            )
type PP a = StateT PPSt
              (ReaderT Int                   -- current level of indentation
                  (Writer [String])) a   -- output

showSSM :: Show a => SSM a -> String
showSSM ssma = let rd             = execStateT (printProcedure ssma) (0, [], [])
                   wr             = runReaderT rd 0
                   ((_,_,funs),_) = runWriter wr
               in unlines funs

printProcedure :: SSM a -> PP ()
printProcedure ssm@(Procedure n _ _) = do
    (_,wr,_) <- get
    if n `elem` wr
        then return ()
        else do modify $ \(i,na,o) -> (i, n:na, o)
                f <- censor (const []) $ snd <$> listen (local (const 0) (toString' ssm))
                modify $ \(i,na,o) -> (i,na,o ++ [unlines f])
  where
      toString' :: SSM a -> PP ()
      toString' ssm = case ssm of
          (Return x)        -> return ()
          (NewRef n e k)    -> do
              emit $ n ++ " = NewRef " ++ show e
              toString' (k n)
          (SetRef r e k)    -> do
              emit $ "SetRef " ++ r ++ " = " ++ show e
              toString' (k ())
          (SetLocal e v k)  -> do
              emit $ "SetLocal " ++ show e ++ " = " ++ show v
              toString' (k ())
          (GetRef r k)      -> do
              v <- getExpString
              emit $ show v ++ " = GetRef " ++ r
              toString' (k v)
          (If c thn (Just els) k)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' thn
              emit   "Else"
              indent $ toString' els
              toString' (k ())
          (If c thn Nothing k)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' thn
              toString' (k ())
          (While c bdy k)   -> do
              emit $ "While " ++ show c
              indent $ toString' bdy
              toString' (k ())
          (After e r v k)   -> do
              emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
              toString' (k ())
          (Changed r k)     -> do
              b <- getExpString
              emit $ show b ++ " = @" ++ r
              toString' (k b) 
          (Wait vars k)     -> do
              emit $ "Wait [" ++ intercalate ", " vars ++ "]"
              toString' (k ())
          (Fork procs k)    -> do
              i <- ask
              {- the following two lines create the separator to use between the forked calls.
              This is used to make a fork rather than rendering like this
                  fork [f a b, f a b, f a b]
              render like this
                  fork [
                         f a b
                       , f a b
                       , f a b
                       ]
              which is a little easier to read. Also might look more like the haskell code (at least
              the way I like to write it).
              -}
              let indent = replicate (i + length "fork ") ' '
              let sep = '\n' : indent
              let calls = map printProcedureCall procs
              emit $ "Fork [ " ++ sep ++ "  " ++ intercalate (sep ++ ", ") calls ++ sep ++ "]"
              mapM_ printProcedure procs
              toString' (k ())
          (Procedure n _ k) -> do
              emit $ "Procedure " ++ n
              toString' (k ())
          (Argument n name (Left e) k)  -> do
              emit $ "Argument " ++ name
              toString' (k ())
          (Argument n name (Right r) k)  -> do
              emit $ "Argument &" ++ name
              toString' (k ())
          (Result n r k)    -> do
              emit $ "Result " ++ show r
              toString' (k ())
        
      getExpString :: PP SSMExp
      getExpString = do
          (i,_,_) <- get
          modify $ \(i,n,w) -> (i+1,n,w)
          return $ Var $ "v" ++ show i

      printProcedureCall :: SSM () -> String
      printProcedureCall (Procedure n _ k) = n ++ "(" ++ intercalate ", " args ++ ")"
          where
              getArgs :: SSM () -> [String]
              getArgs (Argument _ _ (Left e) k)  = show e : getArgs (k ())
              getArgs (Argument _ _ (Right n) k) = n      : getArgs (k ())
              getArgs _                          = []

              args :: [String]
              args = getArgs (k ())

      indent :: PP () -> PP ()
      indent pp = local (+2) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]

-- interpreter

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

---------- code generation ----------

data CGenSt = CGenSt { widestWait  :: Int
                     , nextCase :: Int
                     , nameGen :: Int
                     }
type CGen a = StateT CGenSt -- widest wait encountered so far (determines the number of triggers) 
                (WriterT [String] 
                  IO) a

runCGen :: SSM () -> IO String
runCGen ssm = do
    let rw = evalStateT (genProcedure ssm) (CGenSt 0 0 0)
    out <- execWriterT rw
    return $ unlines out

-- f <- censor (const []) $ snd <$> listen (local (const 0) (compileSSM ssm))
genProcedure :: SSM () -> CGen ()
genProcedure ssm = do
    struct <- censor (const []) $ snd <$> listen (genStruct ssm)
    enter  <- censor (const []) $ snd <$> listen (genEnter ssm)
    step   <- censor (const []) $ snd <$> listen (genStep ssm)
    tell [unlines struct]
    tell [""]
    tell [unlines enter]
    tell [""]
    tell [unlines step]

genStruct :: SSM () -> CGen ()
genStruct (Procedure name _ k) = do
    tell ["typedef struct {"]
    indent "/* Generic procedure fields */"
    indent "void (*step)(rar_t *); // Pointer to step function"
    indent "uint16_t pc;           // Saved control state"
    indent "rar_t *caller;         // Caller's activation record"
    indent "uint16_t children;     // Number of running children"
    indent "uint32_t priority;     // Order in the ready queue"
    indent "uint8_t depth;         // Index of LSB of our priority"
    indent "bool scheduled;        // True when in the ready queue"
    indent "/* procedure specific fields */"
    specifics $ k ()
    triggers
    tell ["} rar_" ++ name ++ "_t;"]
  where
      indent :: String -> CGen ()
      indent str = tell [replicate 4 ' ' ++ str]

      specifics :: SSM () -> CGen ()
      specifics ssm = case ssm of
          (Return x)        -> return ()
          (NewRef n e k)    -> do indent $ "cv_int_t " ++ n ++ ";"
                                  specifics (k n)
          (SetRef r e k)    -> specifics (k ())
          (SetLocal e v k)  -> specifics (k ())
          (GetRef r k)      -> specifics (k (Lit (LBool True)))
          (If c thn (Just els) k) -> specifics (thn >> els >> k ())
          (If c thn Nothing k)  -> specifics (thn >> k ())
          (While c bdy k)   -> specifics (bdy >> k ())
          (After e r v k)   -> specifics (k ())
          (Changed r k)     -> specifics (k (Lit (LBool True))) -- dummy bool
          (Wait vars k)     -> do modify $ \st -> st { widestWait = max (widestWait st) (length vars)}
                                  specifics (k ())
          (Fork procs k)    -> specifics (k ())
          (Procedure n _ k) -> specifics (k ())
          (Argument n name (Left e) k)  -> do indent $ "cv_int_t " ++ name ++ ";"
                                              specifics (k ())
          (Argument n name (Right r) k) -> do indent $ "cv_int_t *" ++ name ++ ";"
                                              specifics (k ())
          (Result n r k)    -> specifics (k ())
    
      triggers :: CGen ()
      triggers = do
          st <- gets widestWait
          mapM_ (\i -> indent ("trigger_t trig" ++ show i ++ ";")) [1..st]


genEnter :: SSM () -> CGen ()
genEnter ssm@(Procedure n _ _) = do
    let level = 19 + length n
    tell ["rar_" ++ n ++ "_t *enter_" ++ n ++ "( rar_t* caller"]
    indent level ", uint32_t priority"
    indent level ", uint8_t depth"
    censor closeParen $ mapM_ (\(t,name) -> indent level (", " ++ t ++ name)) $ getArgs ssm
    indent 0 "{"
    indent 4 $ "rar_" ++ n ++ "_t *rar = (rar_" ++ n ++ "_t *)"
    indent 8 $ "enter(sizeof(rar_" ++ n ++ "_t), step_" ++ n ++ ", caller, priority, depth);"
    mapM_ (\(_,name) -> indent 4 ("rar->" ++ name ++ " = " ++ name ++ ";")) $ getArgs ssm
    i <- gets widestWait
    mapM_ initTrigger [1..i]
    indent 0 "}"
  where
    getArgs :: SSM () -> [(String, String)]
    getArgs ssm = case ssm of
        (Procedure n _ k)             -> getArgs $ k ()
        (Argument n name (Left e) k)  -> ("cv_int_t ", name)   : getArgs (k ())
        (Argument n name (Right r) k) -> ("cv_int_t *", name) : getArgs (k ())
        _                             -> []
    
    initTrigger :: Int -> CGen ()
    initTrigger i = indent 4 $ "rar->trig" ++ show i ++ ".rar = (rar_t *) rar;"

    indent :: Int -> String -> CGen ()
    indent level str = tell [replicate level ' ' ++ str]

    closeParen :: [String] -> [String]
    closeParen []     = []
    closeParen [x]    = [x ++ ")"]
    closeParen (x:xs) = x : closeParen xs



foo :: Exp Int -> Ref Int -> SSM ()
foo = box "foo" ["a", "b"] $ \a b -> do
    c <- changed b
    fork [ foo a b
         , foo c b
         ]


genStep :: SSM () -> CGen ()
genStep ssm@(Procedure n _ _) = do
    tell ["void step_" ++ n ++ "(rar_t *gen_rar)"]
    tell ["{"]
    indent 4 $ "rar_" ++ n ++ "_t *rar = (rar_" ++ n ++ "_t *) gen_rar;"
    indent 4   "switch(rar->pc) {"
    newCase 4
    instant 8 ssm
    indent 4   "; }"
    indent 4 $ "leave((rar_t *) rar, sizeof(rar_" ++ n ++ "_t)); // Terminate"
    tell ["}"]
  where
      indent :: Int -> String -> CGen ()
      indent i str = tell [replicate i ' ' ++ str]

      newCase :: Int -> CGen ()
      newCase level = do
          numcase <- gets nextCase
          modify $ \st -> st {nextCase = nextCase st + 1}
          indent level $ "case " ++ show numcase ++ ":"

      instant :: Int -> SSM () -> CGen ()
      instant level ssm = case ssm of
          (Return x)              -> return ()
          (NewRef n e k)          -> do
              indent level $ "initialize_int(&rar->" ++ n ++ ", " ++ compileLit e ++ ");"
              instant level $ k n
          (SetRef r e k)          -> do
              indent level $ "assign_int(&rar->" ++ r ++ ", rar->priority, " ++ compileLit e ++ ");"
              instant level $ k ()
          (SetLocal (Var e) v k)        -> do
              indent level $ "assign_int(&rar->" ++ e ++ ", rar->priority, " ++ compileLit v ++ ");"
              instant level $ k ()
          (GetRef r k)            -> do
              n <- fresh
              indent level $ "int " ++ n ++ " = *(rar->" ++ r ++ "->value);"
              instant level $ k (Var ("rar->" ++ n))
          (If c thn (Just els) k) -> do
              indent level $ "if( " ++ compileLit c ++ " ) {"
              instant (level + 4) thn
              indent level "} else {"
              instant (level + 4) els
              indent level "}"
              instant level $ k ()
          (If c thn Nothing k)    -> do
              indent level $ "if( " ++ compileLit c ++ " ) {"
              instant (level + 4) thn
              indent level "}"
              instant level $ k ()
          (While c bdy k)         -> do
              indent level $ "while ( " ++ compileLit c ++ " ) {"
              instant (level + 4) bdy
              indent level "}"
              instant level $ k ()
          (After e r v k)         -> do
              indent level $ "later_int(rar->" ++ r ++ ", now + " ++ compileLit e ++ ", " ++ compileLit v ++ ");"
              instant level $ k ()
          (Changed r k)           -> do
              b <- fresh
              indent level $ "bool " ++ b ++ " = event_on((cv_t *) rar->" ++ r ++ ");"
              instant level $ k (Var b)
          (Wait vars k)           -> do
              forM_ (zip vars [1..]) $ \(v,i) -> do
                  indent level $ "sensitize((cv_t*) rar->" ++ v ++ ", &rar->trig" ++ show i ++ ");"
                  pc' <- gets nextCase
                  indent level $ "rar->pc = " ++ show pc' ++ ";"
                  indent level   "return;"
                  newCase (level-4)
                  -- desensitize the triggers here
                  instant level $ k ()
          (Fork procs k)          -> do
              if length procs > 1
                then do
                    let new_depth_corr = integerLogBase 2 (toInteger (length procs))
                    indent level $ "uint8_t  new_depth    = rar->depth - " ++ show new_depth_corr ++ ";"
                    indent level   "uint32_t pinc         = 1 << new_depth;"
                    indent level   "uint32_t new_priority = rar->priority;"
                    let forks  = map (compileFork level "fork") procs
                    let forks' = intersperse ["", "new_priority = += pinc;"] forks
                    mapM_ (mapM_ (indent level)) forks'
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                else do
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                    mapM_ (indent level) $ compileFork level "call" (head procs)
              indent level "return;"
              newCase (level-4)
              instant level $ k ()

          (Procedure _ _ k)       -> instant level $ k ()
          (Argument _ _ _ k)      -> instant level $ k ()
          (Result _ _ k)          -> instant level $ k ()

      compileLit :: SSMExp -> String
      compileLit e = case e of
          Var s         -> "rar->" ++ s ++ "->value"
          Lit (LInt i)  -> show i
          Lit (LBool b) -> if b then "true" else "false"
          UOp e Neg     -> "(-" ++ compileLit e ++ ")"
          BOp e1 e2 op  -> "(" ++ compileLit e1 ++ ")" ++ compileOp op ++ "(" ++ compileLit e2 ++ ")"
        where
            compileOp :: BinOp -> String
            compileOp OPlus  = "+"
            compileOp OMinus = "-"
            compileOp OTimes = "*"
            compileOp OLT    = "<"
            compileOp OEQ    = "=="

      compileFork :: Int -> String -> SSM () -> [String]
      compileFork level method ssm@(Procedure n _ k) = 
          let constant_args = [ "(rar_t *) rar"
                              , "new_priority"
                              , "new_depth"
                              ]
              var_args      = getArgs $ k ()
              all_args      = constant_args ++ var_args
              prefix        = method ++ "((rar_t *) enter_" ++ n ++ "( "
              args          = map (\s -> replicate (length prefix - 1) ' ' ++ ", " ++ s) (tail all_args)
          in (prefix ++ head all_args) : args ++ ["));"]
        where
            getArgs :: SSM () -> [String]
            getArgs (Argument _ x (Left e) k)  = compileLit e : getArgs (k ())
            getArgs (Argument _ x (Right r) k) = r            : getArgs (k ())
            getArgs _                          = []

      fresh :: CGen String
      fresh = do
          c <- gets nameGen
          modify $ \st -> st { nameGen = nameGen st + 1}
          return $ "v" ++ show c

examp :: Ref Int -> SSM ()
examp = box "examp" ["a"] $ \a -> do
    loc <- var "loc" (int 0)
    wait [a]

    loc <~ int 42
    after (int 10) a (int 43)
    
    loc' <- deref loc
    fork [foo (int 42) loc']
    
    loc'' <- deref loc
    fork [foo (int 40) loc'', bar (int 42)]
  where
      foo :: Exp Int -> Exp Int -> SSM ()
      foo = box "foo" ["a1","a2"] $ \a1 a2 -> do
          return ()
    
      bar :: Exp Int -> SSM ()
      bar = box "bar" ["a"] $ \a -> do
          return ()