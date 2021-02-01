{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module SSM where

import AST

import Control.Monad.Writer

import Data.List.NonEmpty hiding (unzip, zip)

type SSM a = IO a

newtype Ref a = Ref Var
newtype Exp a = Exp SSMExp
newtype Argument (cc :: CC) a = Argument (Arg cc)

int :: Int -> Exp Int
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

arg :: Argument c a -> SSM (Ref a)
arg = undefined

method :: String -> (a -> b) -> Function (a -> b)
method = undefined

mainprogram :: String -> SSM () -> Function (SSM ())
mainprogram = undefined

data Function a where
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

-- fibonacci example from his paper

{-

Generally about this approach:
    - I have removed the phantom types from the AST and added another component
      to the elements of the AST, with type `Type`. Instead of `LInt 2 :: SSMLit Int`
      we now have `LInt TInt 2 :: SSMLit`. This allows us to generate appropriate code
      at code generation.
      Instead of this frontend defined in this file using the AST directly there is now an
      IR - Exp a, Ref a & Argument c a, which the typed frontend use.
-}

mywait :: Function (Argument ByReference Int -> SSM ())
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
