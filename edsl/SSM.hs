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
var :: String -> SSMExp a -> SSM (SSMExp (Ref a))
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
fork :: [Routine] -> SSM ()
fork = undefined

-- | Conditional execution, with a dangling else
if' :: SSMExp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' = undefined

{- | Need some way to turn variables into expressions...  I am not sure what is best - if we discard
the distinction of variables and expressions and just use expressions, we will be able to
write ill formed expressions? E.g if `wait` did not explicitly say `Ref a` and said
`SSMExp a` instead we could say `wait [2]`, which makes no sense. However, if we don't draw
the distinction we can not say `r :<: 2` without first converting the r to a SSMExp, using
this function -}
valOf :: Ref a -> SSMExp a
valOf = undefined

-- | LT operator
(<:) :: SSMExp a -> SSMExp a -> SSMExp Bool
(<:) = undefined

{- | Defining a procedure.
NOTE: This is what we spoke of Koen, but I am not sure if I am implementing it as you thought about
it. The `a` is polymorphic so it can very well be `a -> b`, which is nice as we can then use a
variable amount of arguments. However, now when it comes to application I am not sure
how it will work! As you see this doesn't return a function, so we can not apply it. Ideally
we'd be able to write e.g `myway r1`. -}
procedure :: String -> (a -> SSM Routine)
procedure = undefined

-- fibonacci example from the paper

mywait :: SSM Routine
mywait = procedure "mywait" $ \(ByRef r) ->
    wait [r]

mysum :: SSM Routine
mysum = procedure "mysum" $ \(ByRef r1) (ByRef r2) (ByRef r) -> do
  --fork [ mywait r1
  --     , mywait r2
  --     ]
  after (int 1) r (Var r1 `add` Var r2)

myfib :: SSM Routine
myfib = procedure "myfib" $ \(ByVal n) (ByRef r) -> do
  r1 <- var "r1" (int 0)
  r2 <- var "r2" (int 0)
  if' (valOf n <: int 2)
          (after (int 1) r (int 1))
          Nothing -- this should not be here, and the below should be uncommented
          -- fork [ myfib (valOf n - int 1) r1
          --      , myfib (valOf n - int 2) r2
          --      , sum r1 r2 r
          --      ]

mymain :: SSM Routine
mymain = procedure "mymain" $ \() -> do
    r <- var "r" (int 0)
    -- fork [myfib (int 13) r]
    undefined