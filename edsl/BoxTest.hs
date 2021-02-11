{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BoxTest where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.List
import Data.IORef

data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a

    -- | Variable/Stream operations
    NewRef :: String -> SSMExp -> ((String, IORef SSMExp) -> SSM b) -> SSM b
    SetRef :: (String, IORef SSMExp) -> SSMExp -> (() -> SSM b) -> SSM b
    GetRef :: (String, IORef SSMExp) -> (SSMExp -> SSM b) -> SSM b
    
    -- | Control operations
    If     :: Show a => SSMExp -> SSM a -> Maybe (SSM a) -> (() -> SSM b) -> SSM b
    While  :: Show a => SSMExp -> SSM a -> (() -> SSM b) -> SSM b
    
    -- | SSM specific operations
    After  :: SSMExp -> (String, IORef SSMExp) -> SSMExp -> (() -> SSM b) -> SSM b
    Changed :: (String, IORef SSMExp) -> (SSMExp -> SSM b) -> SSM b
    Wait   :: [(String, IORef SSMExp)] -> (() -> SSM b) -> SSM b
    Fork   :: [SSM ()] -> (() -> SSM b) -> SSM b

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> (() -> SSM c) -> SSM c
    Argument  :: String -> String -> Either SSMExp (String, IORef SSMExp) -> (() -> SSM b) -> SSM b
    Result    :: (Show a, Res a) => String -> a -> (() -> SSM b) -> SSM b
--    Tag :: String -> (() -> SSM b) -> SSM b




instance Monad SSM where
    return = Return

    Return x          >>= fa = fa x
    NewRef n e k      >>= fa = NewRef n e      (\x -> k x >>= fa)
    SetRef r e k      >>= fa = SetRef r e      (\x -> k x >>= fa)
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

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp = At  -- ^ @-operator
  deriving (Eq)

instance Show UnaryOp where
    show At = "@"

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

type Ref a = (String, IORef SSMExp) -- references that are shared, (variable name, ref to value)
type Exp a = SSMExp                 -- expressions

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name (x:xs) b = Argument name x (Left b) return >> return (Var x, xs)

instance {-# OVERLAPPING #-} Arg (Ref a) where
    arg name (x:xs) e = Argument name x (Right e) return >> return (e, xs)

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () return >> return ()

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

mywait :: Ref Int -> SSM ()
mywait = box "mywait" ["r"] $ \r -> do
    wait [r]

{-mysum r1 r2 r = do
    fork (mywait r1) (mywait r2)
    v1 <- deref r1
    v2 <- deref r2
    after 1 r (r1 + r2)-}

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
    if' (n <. (int 2))
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) r1
                        , myfib (n -. int 2) r2
                        , mysum r1 r2 r
                        ]))

mymain :: SSM ()
mymain = var "r" (int 0) >>= \r -> fork [myfib (int 13) r]

test :: Exp Int -> SSM ()
test = box "test" ["v"] $ \v ->
    if' (v <. (int 2))
      (fork [test v])
      (Just (fork [test v]))

type PP a = StateT Int                 -- counter to generate fresh names
              (ReaderT Int             -- current level of indentation
                  (Writer [String])) a -- output

showSSM :: Show a => SSM a -> String
showSSM ssma = let rd = evalStateT (toString' ssma) 0
                   wr = runReaderT rd 0
                   w  = execWriter wr
               in unlines w

{-
NOTE: When generating a 'reference' to prettyprint the actual IORef is left as undefined.
This works because to prettyprint it is not necessary to inspect it, only the variable name.
-}

toString' :: SSM a -> PP ()
toString' ssm = case ssm of
    (Return x)        -> return ()
    (NewRef n e k)    -> do
        r@(n,_) <- getRefString
        emit $ n ++ " = NewRef " ++ show e
        toString' (k r)
    (SetRef (r,_) e k)    -> do
        emit $ "SetRef " ++ r ++ " = " ++ show e
        toString' (k ())
    (GetRef (r,_) k)      -> do
        v <- getExpString
        emit $ show v ++ " = GetRef " ++ r
        toString' (k v)
    (If c thn (Just els) k)  -> do
        emit $ "If " ++ show c
        emit $ "Then"
        indent $ toString' thn
        emit $ "Else"
        indent $ toString' els
        toString' (k ())
    (If c thn Nothing k)  -> do
        emit $ "If " ++ show c
        emit $ "Then"
        indent $ toString' thn
        toString' (k ())
    (While c bdy k)   -> do
        emit $ "While " ++ show c
        indent $ toString' bdy
        toString' (k ())
    (After e (r,_) v k)   -> do
        emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
        toString' (k ())
    (Changed (r,_) k)     -> do
        b <- getExpString
        emit $ show b ++ " = @" ++ r
        toString' (k b) 
    (Wait vars k)     -> do
        emit $ "Wait [" ++ intercalate ", " (map fst vars) ++ "]"
        toString' (k ())
    (Fork procs k)    -> do
        emit $ "Fork [" ++ intercalate "," (map printProcedureCall procs) ++ "]"
        toString' (k ())
    (Procedure n _ k) -> do
        emit $ "Procedure " ++ n
        toString' (k ())
    (Argument n name (Left e) k)  -> do
        emit $ "Argument " ++ name
        toString' (k ())
    (Argument n name (Right (r,_)) k)  -> do
        emit $ "Argument &" ++ name
        toString' (k ())
    (Result n r k)    -> do
        emit $ "Result " ++ show r
        toString' (k ())
  where
      getExpString :: PP SSMExp
      getExpString = do
          i <- get
          put $ i + 1
          return $ Var $ "v" ++ show i

      -- undefined for now
      getRefString :: PP (String, IORef SSMExp)
      getRefString = do
          i <- get
          put $ i + 1
          return $ ("v" ++ show i, undefined)

      printProcedureCall :: SSM () -> String
      printProcedureCall (Procedure n _ k) = n ++ "(" ++ intercalate ", " args ++ ")"
        where
            getArgs :: SSM () -> [String]
            getArgs (Argument _ _ (Left e) k)      = (show e) : getArgs (k ())
            getArgs (Argument _ _ (Right (n,_)) k) = n : getArgs (k ())
            getArgs _                              = []

            args :: [String]
            args = getArgs (k ())

      indent :: PP () -> PP ()
      indent pp = local (+4) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]

      result :: String -> PP String
      result str = do
          ind <- ask
          return $ replicate ind ' ' ++ str