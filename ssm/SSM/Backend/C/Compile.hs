-- | Interface module to the Backend.C subsystem.
module SSM.Backend.C.Compile
  ( compile
  ) where

import           SSM.Backend.C.CodeGen
import           SSM.Core.Syntax

import           Data.List                      ( nub )

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )

-- | Compile a program from its Core.Syntax representation to a C String.
compile :: Program -> String
compile p = pretty 120 $ pprList compilationUnit
 where
  compilationUnit = nub includes ++ prg
  (prg, includes) = compile_ p
