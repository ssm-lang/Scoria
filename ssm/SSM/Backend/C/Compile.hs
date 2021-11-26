-- | Interface module to the Backend.C subsystem.
{-# LANGUAGE DataKinds #-}
module SSM.Backend.C.Compile
  ( compile
  ) where

import           SSM.Backend.C.CodeGen
import           SSM.Core.Program
import           SSM.Core.Backend

import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( pprList )

-- | Compile a program from its Core.Syntax representation to a C String.
compile :: Program C -> String
compile p = pretty 120 $ pprList compilationUnit
 where
  compilationUnit = includes ++ prg
  (prg, includes) = compile_ p
