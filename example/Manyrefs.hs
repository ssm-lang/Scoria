{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Manyrefs where

import BinderAnn.Monadic
import SSM

example :: Ref Int -> Ref Bool -> Ref Int -> Exp Int -> SSM ()
example = box "example" ["ri1","rb","ri2","n"] $ \ri1 rb ri2 n -> do
    wait [ri1, ri2]
    wait [rb]
    v1 <- deref ri1
    v2 <- deref ri2
    ri1 <~ (v1 +. v2)