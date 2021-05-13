{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Examp2 where

import BinderAnn.Monadic
import SSM
import Data.Int

ex :: Ref Int32 -> SSM ()
ex = box "ex" ["a"] $ \a -> do
    loc <- var 0

    b <- deref a
    c <- deref loc
    if' (b <. c)
      (wait [a])
      (Just (do after (uint64 2) a 5
                wait [a]))
    after (uint64 5) a 10

ex2 :: Ref Int32 -> SSM ()
ex2 = box "ex2" ["a"] $ \a -> do
    loc <- var 0

    b <- deref a
    c <- deref loc
    if' (b <. c)
      (wait [a])
      Nothing
    after (uint64 5) a 10

ex3 :: Ref Int32 -> SSM ()
ex3 = box "ex3" ["a"] $ \a -> do
    loc <- var 0

    b <- deref a
    c <- deref loc
    if' (b <. c)
      (if' true' 
        (wait [loc]) 
        (Just (wait [a])))
      (Just (wait [loc]))
    after (uint64 5) a 10