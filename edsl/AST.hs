{-# LANGUAGE GADTs #-}
module AST where

import Data.IORef

-- | Arguments to our functions
class Arg a where
    arg :: String -> a -> SSM a

instance (Arg a, Arg b) => Arg (a,b) where
    arg name (x,y) = do
        x' <- arg name x
        y' <- arg name y
        return (x',y')

class Res b where
    result :: String -> b -> SSM b

class Box b where
    box :: Arg a => String -> (a -> b) -> (a -> b)

instance (Arg b, Box c) => Box (b -> c) where
    box name f = curry (box name (uncurry f))

instance Res b => Box (SSM b) where
    box name f = \x -> do
        Procedure name f return
        x' <- arg name x
        y' <- f x'
        result name y'
{-

n :: Ref Int -> Ref Int -> SSM ()
n = box "n" $ \r1 r2 -> do
        body

n someref1 someref2

===

Procedure "n" f (\() ->
Argument "n" someref1 (\() ->
Argument "n" someref2 (\() ->
-- body
Result "n" r (\() ->
return ()))))


Tag "myfib" Proc
Tag "x" Arg
Tag "y" Arg
Procedure "n" f (\() ->
Argument "n" someref1 (\() ->
Argument "n" someref2 (\() ->
-- body
Result "n" r (\() ->
return ()))))
-}

type Reference = String

data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a

    -- | Variable/Stream operations
--    NewRef :: String -> SSMExp -> ((String, IORef SSMExp) -> SSM b) -> SSM b
    NewRef :: String -> SSMExp -> (Reference -> SSM b) -> SSM b
--    SetRef :: (String, IORef SSMExp) -> SSMExp -> (() -> SSM b) -> SSM b
    SetRef :: Reference -> SSMExp -> (() -> SSM b) -> SSM b
--    GetRef :: (String, IORef SSMExp) -> (SSMExp -> SSM b) -> SSM b
    GetRef :: Reference -> (SSMExp -> SSM b) -> SSM b

    -- | Control operations
    If     :: Show a => SSMExp -> SSM a -> Maybe (SSM a) -> (() -> SSM b) -> SSM b
    While  :: Show a => SSMExp -> SSM a -> (() -> SSM b) -> SSM b
    
    -- | SSM specific operations
--    After  :: SSMExp -> (String, IORef SSMExp) -> SSMExp -> (() -> SSM b) -> SSM b
    After  :: SSMExp -> Reference -> SSMExp -> (() -> SSM b) -> SSM b
--    Changed :: (String, IORef SSMExp) -> (SSMExp -> SSM b) -> SSM b
    Changed :: Reference -> (SSMExp -> SSM b) -> SSM b
--    Wait   :: [(String, IORef SSMExp)] -> (() -> SSM b) -> SSM b
    Wait   :: [Reference] -> (() -> SSM b) -> SSM b
    Fork   :: [SSM ()] -> (() -> SSM b) -> SSM b

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> (() -> SSM c) -> SSM c
--    Argument  :: String -> Either SSMExp (String, IORef SSMExp) -> (() -> SSM b) -> SSM b
    Argument  :: String -> Either SSMExp Reference -> (() -> SSM b) -> SSM b
    Result    :: (Show a, Res a) => String -> a -> (() -> SSM b) -> SSM b
--    Tag :: String -> (() -> SSM b) -> SSM b




instance Monad SSM where
    return = Return

    Return x        >>= fa = fa x
    NewRef n e k    >>= fa = NewRef n e    (\x -> k x >>= fa)
    SetRef r e k    >>= fa = SetRef r e    (\x -> k x >>= fa)
    GetRef r k      >>= fa = GetRef r      (\x -> k x >>= fa)
    If c thn els k  >>= fa = If c thn els  (\x -> k x >>= fa)
    While c bdy k   >>= fa = While c bdy   (\x -> k x >>= fa)
    After e r v k   >>= fa = After e r v   (\x -> k x >>= fa)
    Changed r k     >>= fa = Changed r     (\x -> k x >>= fa)
    Wait vars k     >>= fa = Wait vars     (\x -> k x >>= fa)
    Fork procs k    >>= fa = Fork procs    (\x -> k x >>= fa)
    Procedure n f k >>= fa = Procedure n f (\x -> k x >>= fa)
    Argument n a k  >>= fa = Argument n a  (\x -> k x >>= fa)
    Result n r k    >>= fa = Result n r    (\x -> k x >>= fa)

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
