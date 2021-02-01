{-# LANGUAGE GADTs #-}
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
var :: String -> SSMExp a -> SSM (Ref a)    
var = undefined

{- | Need some way to turn variables into expressions...  I am not sure what is best - if we discard
the distinction of variables and expressions and just use expressions, we will be able to
write ill formed expressions? E.g if `wait` did not explicitly say `Ref a` and said
`SSMExp a` instead we could say `wait [2]`, which makes no sense. However, if we don't draw
the distinction we can not say `r :<: 2` without first converting the r to a SSMExp, using
this function -}
valOf :: Ref a -> SSMExp a
valOf = undefined

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

-- | LT operator
(<:) :: SSMExp a -> SSMExp a -> SSMExp Bool
(<:) = undefined

data Function a where
    Name   :: String -> (Arg a -> b) -> Function (Arg a -> b)
    CByRef :: Function (Arg a -> b) -> Ref a -> Function b
    CByVal :: Function (Arg a -> b) -> SSMExp a -> Function b

class App tycon where
    app :: Function (Arg a -> b) -> tycon a -> Function b

instance App Ref where
    app = CByRef

instance App SSMExp where
    app = CByVal

method :: String -> (a -> b) -> Function (a -> b)
method = undefined

mainprogram :: String -> SSM () -> Function (SSM ())
mainprogram = undefined

-- fibonacci example from his paper

{-
Generally about this approach:
  - Several variants of input arguments, can obviously not match on `ByRef` and expect
    it to work. We would like abstraction and application to work seamlessly without having
    to care about if the argument is passed by reference or not.
  - We don't necessarily view references as expressions, so whenever we want to e.g add
    two expressions where one of them is a variable, we need a 'getter' that turns the
    `Ref a` into a `SSMExp a`. In some places in the syntax we explicitly only allow references,
    so we can catch potential type errors, at the cost of readability (the code will be cluttered).
  - Need a type class to more seamlessly be able to apply either references or expressions to
    functions.

-}

mywait :: Function (Arg Int -> SSM ())
mywait = method "mywait" $ \(ByRef r) ->
    wait [r]

mysum :: Function (Arg Int -> Arg Int -> Arg Int -> SSM ())
mysum = method "mysum" $ \(ByRef r1) (ByRef r2) (ByRef r) -> do
    fork [ mywait `app` r1
         , mywait `app` r2
         ]
    after (int 1) r (valOf r1 + valOf r2)

myfib :: Function (Arg Int -> Arg Int -> SSM ())
myfib = method "myfib" $ \(ByVal n) (ByRef r) -> do
  r1 <- var "r1" (int 0)
  r2 <- var "r2" (int 0)
  if' (valOf n <: int 2)
          (after (int 1) r (int 1))
          (Just (fork [ myfib `app` (valOf n - int 1) `app` r1
                      , myfib `app` (valOf n - int 2) `app` r2
                      , mysum `app` r1 `app` r2 `app` r
                      ]))

mymain :: Function (SSM ())
mymain = mainprogram "mymain" $ do
    r <- var "r" (int 0)
    fork [myfib `app` int 13 `app` r]
    undefined

-- fibonacci example from his paper

{-mywait :: Arg a -> SSM ()--SSM Routine
mywait = procedure "mywait" $ \(ByRef r) ->
    wait [r]

mysum :: Arg Int -> Arg Int -> Arg Int -> SSM ()--SSM Routine
mysum = procedure "mysum" $ \(ByRef r1) (ByRef r2) (ByRef r) -> do
  fork [ mywait (ByRef r1)
       , mywait (ByRef r2)
       ]
  after (int 1) r (Var r1 `add` Var r2)

{-
mysum :: exp Int -> exp Int -> exp Int -> SSM ()
mysum = procedure $ \r1 r2 r ->
    fork [ mywait ?
         , mywait ?
         ]
    after 1 r (r1 + r2)

-}

myfib :: Arg Int -> Arg Int -> SSM ()--SSM Routine
myfib = procedure "myfib" $ \(ByVal n) (ByRef r) -> do
  r1 <- var "r1" (int 0)
  r2 <- var "r2" (int 0)
  if' (valOf n <: int 2)
          (after (int 1) r (int 1))
          (Just (fork [ myfib (litByVal (valOf n - int 1)) (ByRef r1)
                      , myfib (litByVal (valOf n - int 2)) (ByRef r2)
                      , sum (ByRef r1) (ByRef r2) (ByRef r)
                      ]))

mymain :: () -> SSM ()--SSM Routine
mymain = procedure "mymain" $ \() -> do
    r <- var "r" (int 0)
    -- fork [myfib (int 13)]
    undefined
-}