{-# LANGUAGE GADTs#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Core where

import BinderAnn.Monadic

type Reference = String
type Name = Maybe ((Int, Int), String)

data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a

    -- | Variable/Stream operations
    NewRef   :: Name -> SSMExp -> (Reference -> SSM b) -> SSM b
    SetRef   :: Reference -> SSMExp -> (() -> SSM b) -> SSM b
    GetRef   :: Reference -> Name -> (SSMExp -> SSM b) -> SSM b
    SetLocal :: SSMExp -> SSMExp -> (() -> SSM b) -> SSM b
    
    -- | Control operations
    If     :: SSMExp -> SSM () -> Maybe (SSM ()) -> (() -> SSM b) -> SSM b
    While  :: SSMExp -> SSM () -> (() -> SSM b) -> SSM b
    
    -- | SSM specific operations
    After   :: SSMExp -> Reference -> SSMExp -> (() -> SSM b) -> SSM b
    Changed :: Reference -> Name -> (SSMExp -> SSM b) -> SSM b
    Wait    :: [Reference] -> (() -> SSM b) -> SSM b
    Fork    :: [SSM ()] -> (() -> SSM b) -> SSM b

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> (() -> SSM c) -> SSM c
    Argument  :: String -> String -> Either SSMExp Reference -> (() -> SSM b) -> SSM b
    Result    :: (Show a, Res a) => String -> a -> (() -> SSM b) -> SSM b
--    Tag :: String -> (() -> SSM b) -> SSM b

instance AnnotatedM SSM a where
    annotateM (NewRef _ e k) info = let (Info (Just name) (Just (_, x, y))) = info
                                    in NewRef (Just ((x,y), name)) e k
    annotateM (GetRef r _ k) info = let (Info (Just name) (Just (_, x, y))) = info
                                    in GetRef r (Just ((x,y), name)) k
    annotateM (Changed r _ k) info = let (Info (Just name) (Just (_, x, y))) = info
                                     in Changed r (Just ((x,y), name)) k
    annotateM ma _                 = ma

instance Monad SSM where
    return = Return

    Return x          >>= fa = fa x
    NewRef n e k      >>= fa = NewRef n e      (\x -> k x >>= fa)
    SetRef r e k      >>= fa = SetRef r e      (\x -> k x >>= fa)
    SetLocal e v k    >>= fa = SetLocal e v    (\x -> k x >>= fa)
    GetRef r s k      >>= fa = GetRef r s      (\x -> k x >>= fa)
    If c thn els k    >>= fa = If c thn els    (\x -> k x >>= fa)
    While c bdy k     >>= fa = While c bdy     (\x -> k x >>= fa)
    After e r v k     >>= fa = After e r v     (\x -> k x >>= fa)
    Changed r s k     >>= fa = Changed r s     (\x -> k x >>= fa)
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