module SSM.Compile where

import SSM.Frontend.Syntax

import SSM.Backend.C.Compile

{- | Compile a program to a C-file
These parameters should be broken out and handled differently. -}
toC :: SSM () -> Bool -> Maybe Int -> String
toC ssm b mi = compile (transpile ssm) b mi