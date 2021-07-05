{- | This module exposes some helpful utility functions used by the quickcheck
machinery. -}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.SSM.QuickCheck.Util
    ( transformProcedures
    , distributeMutate
    , for
    , removeNth
    ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import qualified Data.Map as Map

{- | This function takes a function that returns a list of shrunk `Procedure`s, and
applies it to all procedures in the `Program`. For each new mutated procedure, a
new program is produced. In each new `Program`, only one procedure has been mutated.

Example: You have a program with 5 procedures and they can all be shrunk twice each. The
result of calling @transformProcedures@ will be a list of 10 new programs. -}
transformProcedures :: (Procedure -> [Procedure]) -> Program -> [Program]
transformProcedures tr prg = [ prg { funs = Map.insert n fun' (funs prg) }
                             | (n,fun) <- Map.toList (funs prg)
                             , fun' <- tr fun
                             ]

{- | Distribute a mutation function over a list of elements.

Example:
>>> distributeMutate [1,2,3] (\x -> if even x then [5,6] else [])
>>> [[1,5,3], [1,6,3]]
-}
distributeMutate :: forall a . [a] -> (a -> [a]) -> [[a]]
distributeMutate xs f = map fromHughes $ go (emptyHughes, xs)
  where
      go :: (Hughes a, [a]) -> [Hughes a]
      go (_, [])           = []
      go (current, (y:ys)) = [ current <> (cons_ y' ys) | y' <- f y ] ++
                             go (snoc current y, ys)

{- | Flip the arguments to map - in some cases it's a bit nicer on the eyes. Especially
in the function is more than a few characters big. -}
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Remove the n:th element from a list, with the first element being indexed as 0.
removeNth :: Show a => Int -> [a] -> [a]
removeNth 0 (_:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) xs
removeNth _ []     = error "can not remove from empty list"
