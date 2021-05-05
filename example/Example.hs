{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Example where

import BinderAnn.Monadic
import Core
import Frontend

f2 :: Ref Bool -> Ref Int -> SSM ()
f2 = box "f2" ["ref1", "ref2"] $ \ref1 ref2 -> do
  v0 <- deref ref1
  v1 <- var $ negate ((negate 1) + 0 * (negate 1))
  return ()

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