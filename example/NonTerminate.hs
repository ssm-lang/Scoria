{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module NonTerminate where

import BinderAnn.Monadic
import Frontend

nonterminate :: Ref Int -> Exp Int -> SSM ()
nonterminate = box "nonterminate" ["x", "y"] $ \x y -> do
    v <- deref x
    after (uint64 1) x (v +. (int 1))
    wait [x]
    fork [nonterminate x (v + y)]

stackoverflow :: Ref Int -> SSM ()
stackoverflow = box "stackoverflow" ["r"] $ \r -> do
    fork [ stackoverflow r ]