module SSM.Compile where

import SSM.Core.Syntax

import SSM.Backend.C.Compile

{- | Compile a program to a C-file
These parameters should be broken out and handled differently. -}
toC :: SSMProgram a => a -> Bool -> Maybe Int -> String
toC p b mi = compile (toProgram p) b mi
