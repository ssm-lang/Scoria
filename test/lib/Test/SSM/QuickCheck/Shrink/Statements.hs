module Test.SSM.QuickCheck.Shrink.Statements
  ( statements
  ) where

import           SSM.Core.Syntax
import           SSM.Util.HughesList     hiding ( (++) )

import           Test.SSM.QuickCheck.Util

statements :: Program -> [Program]
statements = transformProcedures shrinkAllStmtsProcedure

-- | Return a list of new procedures where the procedure is mutated by removing
-- statements.
shrinkAllStmtsProcedure :: Procedure -> [Procedure]
shrinkAllStmtsProcedure p =
  [ p { body = (removeLonelyYield body') }
  | body' <- shrinkStmts (emptyHughes, (body p))
  ]

{- | This function will take a procedure body and shrink it by removing those statements
whose type is something like `SSM ()` -- those statement you can delete without making
the remainder of the program fail typechecking.

We do need to pay some consideration when deleting `SSM.Core.Syntax.Stm.SetRef`. If we
blindly try to delete all of these, we will run into some errors. The
`SSM.Frontend.Language.var` function will boil down to a `SSM.Core.Syntax.Stm.CreateRef`
followed by a `SSM.Core.Syntax.Stm.SetRef`. If we remove that `SetRef`, we will have
an uninitialized value, which can lead to undefined behaviour.

What this function does is that it returns mutations where all `SetRef`s are subject
to removal, except if that `SetRef` is preceeded by a corresponding `CreateRef`. In that
case we just leave it untouched.
-}
shrinkStmts :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkStmts (_, []) = []
shrinkStmts (front, (stm1@(CreateRef r1 t) : stm2@(SetRef r2 e) : xs)) =
  if r1 == refIdent r2
    then shrinkStmts (snoc (snoc front stm1) stm2, xs)
    else shrinkStmts (snoc front stm1, stm2 : xs)
shrinkStmts (front, (x : xs)) = case x of
  SetLocal _ _ _ -> (fromHughes front ++ xs) : shrinkStmts (snoc front x, xs)
  SetRef _ _     -> (fromHughes front ++ xs) : shrinkStmts (snoc front x, xs)
  After _ _ _    -> (fromHughes front ++ xs) : shrinkStmts (snoc front x, xs)
  Fork _         -> (fromHughes front ++ xs) : shrinkStmts (snoc front x, xs)
  _              -> shrinkStmts (snoc front x, xs)
