{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Examp where

import BinderAnn.Monadic
import SSM

examp :: Ref Int -> SSM ()
examp = box "examp" ["a"] $ \a -> do
    loc <- var (int 0)
    wait [a]

    loc <~ int 42
    after (uint64 10) a (int 43)
    
    dloc <- deref loc
    fork [foo (int 42) dloc]
    
    dloc2 <- deref loc
    fork [foo (int 40) dloc2, bar (int 42)]
  where
      foo :: Exp Int -> Exp Int -> SSM ()
      foo = box "foo" ["a1","a2"] $ \a1 a2 -> do
          return ()
    
      bar :: Exp Int -> SSM ()
      bar = box "bar" ["a"] $ \a -> do
          return ()