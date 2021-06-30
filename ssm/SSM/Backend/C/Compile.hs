module SSM.Backend.C.Compile ( compile ) where

import SSM.Core.Syntax
import SSM.Backend.C.CodeGen
import SSM.Backend.C.MainLoop

import Data.List ( nub )

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )

{- | Compile a program. The bool parameter determines if you want to generate a main
function for testing purposes, and the Maybe Int specifies how many debug items you want
the generated program to output. -}
compile :: Program -> Bool -> Maybe Int -> String
compile p b mi = pretty 120 $ pprList compilationUnit
  where
      compilationUnit = nub includes ++ prg ++ m

      (prg, includes) = compile_ p
      m               = if b then genMain p mi else []
