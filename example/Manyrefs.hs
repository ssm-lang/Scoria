{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Manyrefs where

import BinderAnn.Monadic
import SSM
import Ssm.Compiler.Cli(compileCli)

main :: IO ()
main = compileCli $ example inputref inputref inputref 22

example :: Ref Int32 -> Ref Bool -> Ref Int32 -> Exp Int32 -> SSM ()
example = box "example" ["ri1","rb","ri2","n"] $ \ri1 rb ri2 n -> do
    wait [ri1, ri2]
    wait [rb]
    v1 <- deref ri1
    v2 <- deref ri2
    ri1 <~ (v1 +. v2)
