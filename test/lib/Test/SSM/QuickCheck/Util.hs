{- | This module exposes some helpful utility functions used by the quickcheck
machinery. -}
module Test.SSM.QuickCheck.Util where

import SSM.Core.Syntax

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

{- | Flip the arguments to map - in some cases it's a bit nicer on the eyes. Especially
in the function is more than a few characters big. -}
for :: [a] -> (a -> b) -> [b]
for = flip map
