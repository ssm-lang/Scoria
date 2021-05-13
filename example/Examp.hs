{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Examp where

import BinderAnn.Monadic
import SSM
import Data.Int

examp :: Ref Int32 -> SSM ()
examp = box "examp" ["a"] $ \a -> do
    loc <- var 0
    wait [a]

    loc <~ (42 :: Exp Int32)
    after (uint64 10) a 43
    
    dloc <- deref loc
    fork [foo 42 dloc]
    
    dloc2 <- deref loc
    fork [foo 40 dloc2, bar 42]
  where
      foo :: Exp Int32 -> Exp Int32 -> SSM ()
      foo = box "foo" ["a1","a2"] $ \a1 a2 -> do
          return ()
    
      bar :: Exp Int32 -> SSM ()
      bar = box "bar" ["a"] $ \a -> do
          return ()