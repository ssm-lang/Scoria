{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Buggy where

import BinderAnn.Monadic
import SSM
import Data.Int

fun2 :: Ref Bool -> Ref Bool -> Exp Bool -> SSM ()
fun2 = box "fun2" ["ref1","ref2","var3"] $ \ref1 ref2 var3 -> do
    ref1 <~ false'
    ref1 <~ (((negate 1) :: Exp Int) <. (0 - 0))
    if' (true' ==. var3)
      (return ())
      (Just (return ()))

e1 :: Ref Int64 -> SSM ()
e1 = box "e1" ["ref2"] $ \ref2 -> do
  ref2 <~ (int64 5)
  wait [ref2]
  d <- deref ref2
  ref2 <~ (d + int64 10)

e2 :: Ref Int64 -> SSM ()
e2 = box "e2" ["ref1"] $ \ref1 -> do
  fork [ e1 ref1, e1 ref1 ]

badevent :: Ref Int -> SSM ()
badevent = box "badevent" ["r"] $ \r -> do
  after 1 r 5
  wait [r]
  after 2 r 10

eventorder :: Ref Int -> Ref Int -> SSM ()
eventorder = box "eventorder" ["r1","r2"] $ \r1 r2 -> do
  after 2 r1 5
  after 2 r2 10