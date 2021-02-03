{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM where

import AST hiding (Arg, SSMType)

import Control.Monad.Writer

import Data.IORef

data SSM a
instance Functor SSM
instance Applicative SSM
instance Monad SSM

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



newtype Ref a = Ref (IORef a) -- references that are shared
newtype Exp a = Exp SSMExp    -- expressions


-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name b = undefined

instance Arg (Ref a) where
    arg name e = undefined

-- | Possible results of SSM procedures
instance Res () where
    result = undefined


{- | Containers that we can assign values to. Capture it in this class so that we
can use the same function `assign` regardless of whether we are writing to a reference
or a stream. -}
class Assignable a b where
    assign :: a -> b -> SSM ()

instance Assignable (Exp a) (Exp a) where
    assign = undefined

instance Assignable (Ref (Exp a)) (Exp a) where
    assign = undefined

class BinOps tycon1 tycon2 where
    (+.)  :: tycon1 a -> tycon2 a -> Exp a
    (-.)  :: tycon1 a -> tycon2 a -> Exp a
    (*.)  :: tycon1 a -> tycon2 a -> Exp a
    (<.)  :: tycon1 a -> tycon2 a -> Exp Bool
    (==.) :: tycon1 a -> tycon2 a -> Exp Bool

instance BinOps Exp Exp where
    (Exp e1) +. (Exp e2)  = Exp $ BOp e1 e2 OPlus
    (Exp e1) -. (Exp e2)  = Exp $ BOp e1 e2 OMinus
    (Exp e1) *. (Exp e2)  = Exp $ BOp e1 e2 OTimes
    (Exp e1) <. (Exp e2)  = Exp $ BOp e1 e2 OLT
    (Exp e1) ==. (Exp e2) = Exp $ BOp e1 e2 OEQ

instance Num a => Num (Exp a) where
    (Exp e1) + (Exp e2) = Exp $ BOp e1 e2 OPlus
    (Exp e1) * (Exp e2) = Exp $ BOp e1 e2 OTimes
    abs                 = undefined -- for now
    signum              = undefined -- for now
    fromInteger i       = Exp $ Lit (LInt (fromInteger i))
    negate              = undefined


int :: Int -> Exp Int
int i = Exp $ Lit $ LInt i

-- | Dereference a stream
deref :: Ref a -> a
deref = undefined

-- | Grab the address of a stream
ref :: a -> Ref a
ref = undefined

-- | Grab the value from a stream
--val :: Stream a -> Exp a
--val = undefined

-- | Only way to create a stream (variable)
var :: String -> Exp a -> SSM (Exp a)
var = undefined

-- | Assignment, e.g n `becomes` (r1 + r2)
becomes :: Exp a -> Exp a -> SSM ()
becomes = undefined

-- | Wait for one of many variables to be written to
wait :: [Exp a] -> SSM ()
wait r = undefined

-- | Delayed assignment
after :: Assignable a b => Exp Int -> a -> b -> SSM ()
after = undefined

-- | Create child processes
fork :: [SSM ()] -> SSM ()
fork = undefined

-- The @-operator
changed :: Exp a -> SSM ()
changed = undefined

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = undefined

-- | Repeat computation until condition becomes true
while' :: Exp Bool -> SSM () -> SSM ()
while' = undefined

-- fibonacci example

mywait :: Ref (Exp Int) -> SSM ()
mywait = box "mywait" $ \r -> do
    wait [deref r]

mysum :: Ref (Exp Int) -> Ref (Exp Int) -> Ref (Exp Int) -> SSM ()
mysum = box "mysum" $ \r1 r2 r -> do
    fork [ mywait r1
         , mywait r2
         ]
    after (int 1) r (deref r1 + deref r2)

myfib :: Exp Int -> Ref (Exp Int) -> SSM ()
myfib = box "myfib" $ \n r -> do
    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. (int 2))
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) (ref r1)
                        , myfib (n -. int 2) (ref r2)
                        , mysum (ref r1) (ref r2) r
                        ]))

{-int :: Int -> Exp Int
int i = Exp $ Lit TInt $ LInt TInt i

-- | Declare a variable
var :: String -> Exp a -> SSM (Ref a)    
var name e = undefined

wait :: [Ref a] -> SSM ()
wait r = undefined

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after = undefined

fork :: [Function (SSM ())] -> SSM ()
fork = undefined

-- The @-operator
changed :: Ref a -> SSM ()
changed = undefined

if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = undefined

while' :: Exp Bool -> SSM () -> SSM ()
while' = undefined
-}

{-data Function a where
    Name :: String -> (Argument c a -> b) -> Function (Argument c a -> b)
    App  :: Function (Argument c a -> b) -> Exp a -> Function b

class App tycon where
    app :: SSMType a => Function (Argument c a -> b) -> tycon a -> Function b

instance App Ref where
    app f r'@(Ref r) = let t = typeOf r'
                       in App f (Exp (Var t r))

instance App Exp where
    app = App

class BOps tycon1 tycon2 where
    (+.) :: forall a . SSMType a => tycon1 a -> tycon2 a -> Exp a
    (-.) :: forall a . SSMType a => tycon1 a -> tycon2 a -> Exp a
    (*.) :: forall a . SSMType a => tycon1 a -> tycon2 a -> Exp a
    (<.) :: forall a . SSMType a => tycon1 a -> tycon2 a -> Exp Bool

instance BOps Ref Exp where
    r'@(Ref r) +. (Exp e) = let t = typeOf r'
                            in Exp $ BOp t (Var t r) e OPlus
    r'@(Ref r) -. (Exp e) = let t = typeOf r'
                            in Exp $ BOp t (Var t r) e OMinus
    r'@(Ref r) *. (Exp e) = let t = typeOf r'
                            in Exp $ BOp t (Var t r) e OTimes
    (Ref r) <. (Exp e)    = let t = TBool
                            in Exp $ BOp t (Var t r) e OLT

instance BOps Exp Ref where
    (Exp e) +. r'@(Ref r) = let t = typeOf r'
                            in Exp $ BOp t e (Var t r) OPlus
    (Exp e) -. r'@(Ref r) = let t = typeOf r'
                            in Exp $ BOp t e (Var t r) OMinus
    (Exp e) *. r'@(Ref r) = let t = typeOf r'
                            in Exp $ BOp t e (Var t r) OTimes
    (Exp e) <. (Ref r)    = let t = TBool
                            in Exp $ BOp t e (Var t r) OLT

instance BOps Exp Exp where
    e@(Exp e1) +. (Exp e2) = let t = typeOf e
                             in Exp $ BOp t e1 e2 OPlus
    e@(Exp e1) -. (Exp e2) = let t = typeOf e
                             in Exp $ BOp t e1 e2 OMinus
    e@(Exp e1) *. (Exp e2) = let t = typeOf e
                             in Exp $ BOp t e1 e2 OTimes
    (Exp e1) <. (Exp e2)   = let t = TBool  
                             in Exp $ BOp t e1 e2 OLT

instance BOps Ref Ref where
    r@(Ref r1) +. (Ref r2) = let t = typeOf r
                             in Exp $ BOp t (Var t r1) (Var t r2) OPlus
    r@(Ref r1) -. (Ref r2) = let t = typeOf r
                             in Exp $ BOp t (Var t r1) (Var t r2) OMinus
    r@(Ref r1) *. (Ref r2) = let t = typeOf r
                             in Exp $ BOp t (Var t r1) (Var t r2) OTimes
    (Ref r1) <. (Ref r2)   = let t = TBool
                             in Exp $ BOp t (Var t r1) (Var t r2) OLT
-}

-- fibonacci example from his paper

{-

Generally about this approach:
    - I have removed the phantom types from the AST and added another component
      to the elements of the AST, with type `Type`. Instead of `LInt 2 :: SSMLit Int`
      we now have `LInt TInt 2 :: SSMLit`. This allows us to generate appropriate code
      at code generation.
      Instead of this frontend defined in this file using the AST directly there is now an
      IR - Exp a, Ref a & Argument c a, which the typed frontend use. We can now be specific
      about types when we use the EDSL 
-}

{-mywait :: Function (Argument ByReference Int -> SSM ())
mywait = method "mywait" $ \r' -> do
    r <- arg r'
    wait [r]

mysum :: Function (  Argument ByReference Int 
                  -> Argument ByReference Int 
                  -> Argument ByReference Int 
                  -> SSM ())
mysum = method "mysum" $ \r1' r2' r' -> do
    r1 <- arg r1'
    r2 <- arg r2'
    r  <- arg r'
    fork [ mywait `app` r1
         , mywait `app` r2
         ]
    after (int 1) r (r1 +. r2)

myfib :: Function (  Argument ByValue Int
                  -> Argument ByReference Int
                  -> SSM ())
myfib = method "myfib" $ \n' r' -> do
    n <- arg n'
    r <- arg r'

    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. (int 2))
            (after (int 1) r (int 1))
            (Just (fork [ myfib `app` (n -. int 1) `app` r1
                        , myfib `app` (n -. int 2) `app` r2
                        , mysum `app` r1 `app` r2 `app` r
                        ]))

mymain :: Function (SSM ())
mymain = mainprogram "mymain" $ do
    r <- var "r" (int 0)
    fork [myfib `app` int 13 `app` r]



-- runSSM
-}