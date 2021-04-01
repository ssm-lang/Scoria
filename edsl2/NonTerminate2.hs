{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module NonTerminate2 where

import BinderAnn.Monadic
import Frontend2

nonterminate :: Ref Int -> SSM ()
nonterminate = box "nonterminate" ["x"] $ \x -> do
    v <- deref x
    after (int 1) x (v +. (int 1))
    wait [x]
    fork [nonterminate x]