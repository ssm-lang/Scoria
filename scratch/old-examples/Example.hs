{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Example where

import BinderAnn.Monadic
import Core
import SSM
import Data.Int

f1 :: Ref Int32 -> SSM ()
f1 = box "f1" ["r"] $ \r -> do
  after 1 r 2
  wait [r]
  after 1 r 3
  wait [r]
  after 1 r 4
  wait [r]


example :: Ref Int32 -> SSM ()
example = box "example" ["r"] $ \r -> do
  v <- deref r
  after 1 r (v + 1)
  wait [r]
  fork [example r]

testafter :: Ref Int32 -> SSM ()
testafter = box "testafter" ["r"] $ \r -> do
  after 2 r 5
  after 3 r 10

forktest :: Ref Int32 -> SSM ()
forktest = box "forktest" ["r"] $ \r -> do
  fork [ forktest r, forktest r]