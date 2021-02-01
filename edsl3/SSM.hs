{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module SSM where

import AST

import Data.List.NonEmpty hiding (unzip, zip)

type SSM a = IO a

{- Can not write something like this. Need impredicative types apparently.
   We can only declare variables of the same type, also. -}
{- type SSM a = WriterT ([Ref a, SSMExp a], [SSMStm]) IO a -}

{- | Turn a literal into an expression. I tried to use num instance for SSMExp
but if I write `2` it can not deduce that I mean e.g `2 :: Int` and not `2 :: Integer`,
so I had to annotate the expression.. this looks better than that, at least. -}
int :: Int -> SSMExp Int
int = Lit . LInt

-- | Declare a variable
var :: String -> SSMExp a -> SSM (Ref a)    
var = undefined

{- | Wait for any of the variables in the list to become true. Must be non empty
per the syntax described in the paper. -}
wait :: [Ref a] -> SSM ()
wait r = undefined

-- | Delayed assignment
after :: SSMExp Int -> Ref a -> SSMExp a -> SSM ()
after = undefined

{- | Fork
NOTE: this is not done, I am not entirely sure how to implement what Koen & I spoke of,
so that the type system will make sure the application is well typed. -}
fork :: [Function (SSM ())] -> SSM ()
fork = undefined

-- | Conditional execution, with a dangling else
if' :: SSMExp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = undefined

data Function a where
    Name :: String -> (Arg c a -> b) -> Function (Arg c a -> b)
    App  :: Function (Arg c a -> b) -> SSMExp a -> Function b

class App tycon where
    app :: Function (Arg c a -> b) -> tycon a -> Function b

instance App Ref where
    app f r = App f (Var r)

instance App SSMExp where
    app = App

arg :: Arg c a -> SSM (Ref a)
arg = undefined

method :: String -> (a -> b) -> Function (a -> b)
method = undefined

mainprogram :: String -> SSM () -> Function (SSM ())
mainprogram = undefined

-- fibonacci example from his paper

{-

Generally about this approach:
    - This approach _does_ make a distinction between references and expressions.
      Compared to version 2 I have created a typeclass in AST.hs where I define
      operators such as +. and -.. These allow me to write r +. e regardless the types
      of the operands. We can be very type specific in those places where we want to, and
      not having to care where we don't want to care.
-}

mywait :: Function (Arg ByReference Int -> SSM ())
mywait = method "mywait" $ \r' -> do
    r <- arg r'
    wait [r]

mysum :: Function (Arg ByReference Int -> Arg ByReference Int -> Arg ByReference Int -> SSM ())
mysum = method "mysum" $ \r1' r2' r' -> do
    r1 <- arg r1'
    r2 <- arg r2'
    r  <- arg r'
    fork [ mywait `app` r1
         , mywait `app` r2
         ]
    after (int 1) r (r1 +. r2)

myfib :: Function (Arg ByValue Int -> Arg ByReference Int -> SSM ())
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