module Test.SSM.QuickCheck.Shrink.If
    ( ifs ) where

import SSM.Core.Syntax
import SSM.Core.Program

import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

{- | Shrink If-statements in a SSM program. Every time an If-statement is encountered,
the program is turned into three mutations. One mutation does not have the If-statement
at all, one mutation has the then-branch inlined while the third mutation has the
else-branch inlined. This is done once for each If-statement, so a program with three
If-else-statements will be turned into a total of nine different mutations. -}
ifs :: Program -> [Program]
ifs = transformProcedures shrinkIfProcedure

-- | Shrink a procedure into several (or none) different mutations.
shrinkIfProcedure :: Procedure -> [Procedure]
shrinkIfProcedure p = let bodys = shrinkIfStm (emptyHughes, body p)
                      in for bodys $ \bdy -> p { body = bdy }

shrinkIfStm :: (Hughes Stm,[Stm]) -> [[Stm]]
shrinkIfStm (_,[])           = []
shrinkIfStm (front, (x:xs)) = case x of
  If c thn els ->
      let front' = fromHughes front
          curr   = [front' ++ (Skip:xs), front' ++ thn ++ xs, front' ++ els ++ xs]
      in curr ++ shrinkIfStm (snoc front x, xs)
  While c bdy ->
      let bdys  = shrinkIfStm (emptyHughes, bdy)
          front' = fromHughes front
          curr = [front' ++ (Skip:xs)] ++ [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
      in curr ++ shrinkIfStm (snoc front x, xs)
  _ -> shrinkIfStm (snoc front x, xs)
