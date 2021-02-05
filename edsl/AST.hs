{-# LANGUAGE GADTs #-}
module AST where

import Data.IORef

-- | Arguments to our functions
class Arg a where
    arg :: String -> a -> SSM a

instance (Arg a, Arg b) => Arg (a,b) where
    arg name (x,y) = do
        x' <- arg name x -- TODO is this wrong though? What names should the components have? Surely not the same name.
        y' <- arg name y
        return (x',y')

class Res b where
    result :: String -> b -> SSM b

class Box b where
    box :: Arg a => String -> (a -> b) -> (a -> b)

instance (Arg b, Box c) => Box (b -> c) where
    box name f = curry (box name (uncurry f))

instance Res b => Box (SSM b) where
    box name f = \x ->
        do x' <- arg name x
           y' <- f x'
           result name y'

data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a
    Bind    :: SSM a -> (a -> SSM b) -> SSM b

    -- | Variable/Stream operations
    NewRef  :: String -> SSMExp -> SSM (String, (IORef SSMExp))
    SetRef  :: (String, IORef SSMExp) -> SSMExp -> SSM ()
    GetRef  :: (String, IORef SSMExp) -> SSM SSMExp
    
    -- | Control operations
    If      :: SSMExp -> SSM a -> Maybe (SSM a) -> SSM ()
    While   :: SSMExp -> SSM a -> SSM ()
    
    -- | SSM specific operations
    After   :: SSMExp -> (String, IORef SSMExp) -> SSMExp -> SSM ()
    Changed :: (String, IORef SSMExp) -> SSM SSMExp
    Wait    :: [(String, IORef SSMExp)] -> SSM ()
    Fork    :: [SSM ()] -> SSM ()

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> SSM (a -> b)
    Argument :: String -> Either SSMExp (String, IORef SSMExp) -> SSM ()
    Result    :: Res a => String -> a -> SSM ()

instance Monad SSM where
    return = Return
    (>>=)  = Bind
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
  deriving Show

-- | SSM literals
data SSMLit = LInt Int    -- ^ Integer literals
            | LBool Bool  -- ^ Boolean literals
  deriving Show

{-- | SSM unary operators. We use phantom types to represent the argument type
and the result type of the operator. E.g At (a :: Ref b) :: UnaryOp Bool. -}
data UnaryOp = At  -- ^ @-operator
  deriving Show

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp = OPlus   -- ^ addition
           | OMinus  -- ^ subtraction
           | OTimes  -- ^ multiplication
           | OLT     -- ^ less-than
           | OEQ     -- ^ eq
  deriving Show
