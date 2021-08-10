{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Buggy where

import BinderAnn.Monadic
import SSM

fun2 :: Ref Bool -> Ref Bool -> Exp Bool -> SSM ()
fun2 = box "fun2" ["ref1","ref2","var3"] $ \ref1 ref2 var3 -> do
    ref1 <~ false'
    ref1 <~ (((negate 1) :: Exp Int32) <. (0 - 0))
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

badevent :: Ref Int32 -> SSM ()
badevent = box "badevent" ["r"] $ \r -> do
  after 1 r 5
  wait [r]
  after 2 r 10

eventorder :: Ref Int32 -> Ref Int32 -> SSM ()
eventorder = box "eventorder" ["r1","r2"] $ \r1 r2 -> do
  after 2 r1 5
  after 2 r2 10

infevent :: Ref Int32 -> SSM ()
infevent = box "infevent" ["r"] $ \r -> do
  v0 <- var 5
  after 2 v0 5
  fork [ infevent v0]

badafter :: Ref Int32 -> SSM ()
badafter = box "badafter" ["r"] $ \r -> do
  after 10 r 5
  wait [r]
  after ((uint64 maxBound) - 1) r 10

testlocal :: Ref Bool -> Exp Int32 -> SSM ()
testlocal = box "testlocal" ["r","e"] $ \r e -> do
  e <~ ((1 - e) * (52 * e))
  after 1 r ((217 * e) <. (112 - 20))
  wait [r]

badwait :: Ref Int64 -> Ref Int64 -> SSM ()
badwait = box "badwait" ["ref5","ref8"] $ \ref5 ref8 -> do
  after 2153 ref5 1
  wait [ref5, ref8]
  fork [ badwait ref8 ref8]

sameeventtime :: Ref Int32 -> Ref Int32 -> SSM ()
sameeventtime = box "sameeventtime" ["r1","r2"] $ \r1 r2 -> do
  after 50 r1 5
  after 50 r2 10
  x <- var (5 :: Exp Int32)
  after 20 x 10

{- buggy -}

bfun1 :: Ref Int64 -> Exp Int64 -> SSM ()
bfun1 = box "bfun1" ["ref2","var5"] $ \ref2 var5 -> do
  after 5 ref2 $ (var5 * 1)
  wait [ref2]

bfun3 :: Ref Int64 -> Ref Int64 -> Ref Int64 -> SSM ()
bfun3 = box "bfun3" ["ref3","ref4","ref5"] $ \ref3 ref4 ref5 -> do
  fork [ bfun1 ref4 1]
  fork [ bfun3 ref3 ref3 ref5
       , bfun4 ref5 ref5
       , bfun1 ref4 0
       ]

bfun4 :: Ref Int64 -> Ref Int64 -> SSM ()
bfun4 = box "bfun4" ["ref1","ref4"] $ \ref1 ref4 -> do
  after 1 ref4 1
  wait [ ref1 ]
  fork [ bfun4 ref1 ref4 ]

{- Weird enqueues -}

bbfun1 :: Ref Int64 -> Ref Bool -> SSM ()
bbfun1 = box "bbfun1" ["ref1","ref4"] $ \ref1 ref4 -> do
  after 2 ref1 5
  after 1 ref4 true'
  v1 <- var true'
  wait [ref4]
  fork [ bbfun1 ref1 v1
       , bbfun1 ref1 v1
       , bbfun1 ref1 ref4
       ]

{-
shrinking throws an error at this program, for some reason...
It shrinks it so that it is no longer well formed.

bbfun1 :: Ref Int64 -> Ref Bool -> SSM ()
bbfun1 = box "bbfun1" ["ref1","ref4"] $ \ref1 ref4 -> do
  after 5 ref1 5 -- commenting out this one makes the issue go away
  after 3 ref4 true'
  v1 <- var true'
  wait [ref4]
  fork [ bbfun1 ref1 v1
       , bbfun1 ref1 v1
       , bbfun1 ref1 ref4
       ]
-}
