{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM where

import AST hiding (Arg, SSMType, If, While, SSMStm, Argument, Wait, After, Fork)

import Control.Monad.Writer

import Data.IORef
import Data.Maybe
import Data.List

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



type Ref a = (String, IORef SSMExp) -- references that are shared
type Exp a = SSMExp                 -- expressions


-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name b = Argument name (Left b) >> return b

instance {-# OVERLAPPING #-} Arg (Ref a) where
    arg name e = Argument name (Right e) >> return e

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () >> return ()

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

-- | Dereference a stream
deref :: Ref a -> SSM (Exp a)
deref = GetRef

-- | Only way to create a stream (variable)
var :: String -> Exp a -> SSM (Ref a)
var = NewRef

-- | Wait for one of many variables to be written to
wait :: [Ref a] -> SSM ()
wait = Wait

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after = After

-- | Create child processes
fork :: [SSM ()] -> SSM ()
fork = Fork

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed = Changed

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = If

-- | Repeat computation until condition becomes true
while' :: Exp Bool -> SSM () -> SSM ()
while' = While

-- fibonacci example

mywait :: Ref Int -> SSM ()
mywait = box "mywait" $ \r -> do
    wait [r]

mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" $ \r1 r2 r -> do
    fork [ mywait r1
         , mywait r2
         ]
    v1 <- deref r1
    v2 <- deref r2
    after (int 1) r (v1 +. v2)

myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" $ \n r -> do
    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. (int 2))
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) r1
                        , myfib (n -. int 2) r2
                        , mysum r1 r2 r
                        ]))

mymain :: SSM ()
mymain = var "r" (int 0) >>= \r -> myfib (int 13) r