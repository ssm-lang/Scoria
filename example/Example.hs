{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Example where

import BinderAnn.Monadic
import Core
import SSM

f1 :: Ref Int -> SSM ()
f1 = box "f1" ["r"] $ \r -> do
  after 1 r 2
  wait [r]
  after 1 r 3
  wait [r]
  after 1 r 4
  wait [r]


example :: Ref Int -> SSM ()
example = box "example" ["r"] $ \r -> do
  v <- deref r
  after 1 r (v + 1)
  wait [r]
  fork [example r]

testafter :: Ref Int -> SSM ()
testafter = box "testafter" ["r"] $ \r -> do
  after 2 r 5
  after 3 r 10

forktest :: Ref Int -> SSM ()
forktest = box "forktest" ["r"] $ \r -> do
  fork [ forktest r, forktest r]