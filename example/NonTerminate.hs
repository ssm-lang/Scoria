{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module NonTerminate where

import BinderAnn.Monadic
import SSM
import Data.Int

nonterminate :: Ref Int32 -> Exp Int32 -> SSM ()
nonterminate = box "nonterminate" ["x", "y"] $ \x y -> do
    v <- deref x
    after (uint64 1) x (v +. 1)
    wait [x]
    fork [nonterminate x (v + y)]

stackoverflow :: Ref Int -> SSM ()
stackoverflow = box "stackoverflow" ["r"] $ \r -> do
    fork [ stackoverflow r ]