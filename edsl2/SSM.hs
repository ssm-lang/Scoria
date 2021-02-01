{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module SSM where

import AST

import Data.List.NonEmpty hiding (unzip, zip)

type SSM a = IO a -- placeholder for _some_ monad, TBD at a later time (need to talk with Koen)

{- | Turn a literal into an expression. I tried to use num instance for SSMExp
but if I write `2` it can not deduce that I mean e.g `2 :: Int` and not `2 :: Integer`,
so I had to annotate the expression.. this looks better than that, at least. -}
int :: Int -> SSMExp Int
int = Lit . LInt

-- | Add two expressions of type Int
add :: SSMExp Int -> SSMExp Int -> SSMExp Int
add = undefined

-- | Assign an expression to a variable
(*=) :: SSMExp (Ref a) -> SSMExp a -> SSM ()
r *= e = undefined

-- | Declare a variable
var :: String -> SSMExp a -> SSM (SSMExp a)    
var = undefined

{- | Wait for any of the variables in the list to become true. Must be non empty
per the syntax described in the paper. -}
wait :: [SSMExp a] -> SSM ()
wait r = undefined

-- | Delayed assignment
after :: SSMExp Int -> SSMExp a -> SSMExp a -> SSM ()
after = undefined

{- | Fork
NOTE: this is not done, I am not entirely sure how to implement what Koen & I spoke of,
so that the type system will make sure the application is well typed. -}
fork :: [Function (SSM ())] -> SSM ()
fork = undefined

-- | Conditional execution, with a dangling else
if' :: SSMExp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = undefined

-- | LT operator
(<:) :: SSMExp a -> SSMExp a -> SSMExp Bool
(<:) = undefined

data Function a where
    Name :: String -> (Arg c a -> b) -> Function (Arg c a -> b)
    App  :: Function (Arg c a -> b) -> SSMExp a -> Function b

app :: Function (Arg c a -> b) -> SSMExp a -> Function b
app = App

arg :: Arg c a -> SSM (SSMExp a)
arg = undefined

method :: String -> (a -> b) -> Function (a -> b)
method = undefined

mainprogram :: String -> SSM () -> Function (SSM ())
mainprogram = undefined

-- fibonacci example from his paper

{-

Generally about this approach:
  - No non-enhaustiveness when passing arguments, and the value-or-reference variant
    of the arguments is captured in the type. When generating code for this we can
    perhaps do something like this to treat the arguments differently depending on
    if they are pass by value or reference (If need be?):

    class Render a where
      render :: a -> String

    instance Render (Arg ByValue a) where
      render (Arg e) = undefined

    instance Render (Arg ByReference a) where
      render (Arg e) = undefined

    We can 'extract' the expression from the argument by calling e.g arg. If the argument was
    a literal (e.g 2) we still want to treat it as a variable in our program (except when interpreting).
    Doing this also enables us to capture the variable name for code generation using binderann.
    It introduces some unecessary lines though. Ideally we'd just write \r1 r2 r -> and then use
    them like that.

  - Here I changed all references to be of type `SSMExp a` instead of `Ref a`. We get rid of
    `valOf` and we can write e.g `r1 + r2` without having to turn them into expressions.
    The syntax e.g explicitly states that `after` should have a type like
    `SSMExp Int -> Ref a -> SSMExp a -> SSM ()`, but now we removed our `Ref a`, so we nee
    to have `SSMExp Int -> SSMExp a -> SSMExp a -> SSM ()` instead, and then check that the
    first expression is a reference at a later stage instead, not making full use of the typechecker.
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
    after (int 1) r (r1 + r2)

myfib :: Function (Arg ByValue Int -> Arg ByReference Int -> SSM ())
myfib = method "myfib" $ \n' r' -> do
    n <- arg n'
    r <- arg r'

    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <: int 2)
            (after (int 1) r (int 1))
            (Just (fork [ myfib `app` (n - int 1) `app` r1
                        , myfib `app` (n - int 2) `app` r2
                        , mysum `app` r1 `app` r2 `app` r
                        ]))

mymain :: Function (SSM ())
mymain = mainprogram "mymain" $ do
    r <- var "r" (int 0)
    fork [myfib `app` int 13 `app` r]