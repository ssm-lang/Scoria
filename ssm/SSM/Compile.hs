{- | Interface to the SSM compilation subsystem. Exports functions which
    compile programs written in the SSM EDSL to C code.

Right now what you can do is to load this module into ghci and evaluate:

>>> writeFile "filename.c" $ toC program

which will give you a C-file that contains your program. Now you can manually
place this where it needs to go, until we finish the end-to-end compiler
pipeline.
-}
module SSM.Compile where

import           SSM.Backend.C.Compile
import           SSM.Core.Syntax

-- | Compile a program to a C-file.
toC :: SSMProgram a => a -> String
toC = compile . toProgram
