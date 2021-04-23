{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Buggy where

import BinderAnn.Monadic
import Frontend

fun1 :: Exp Int -> Exp Int -> Ref Int -> SSM ()
fun1 = box "fun1" ["var1","var2","ref3"] $ \var1 var2 ref3 -> do
    var1 <~ var2
    var2 <~ (negate $ negate var1)
    after (((var1 + 3) * (var2 + var2)) * ((var1 + 3) * (var2 + var2)) + 1) ref3 ((var1 * 1) + (var1 * var2))

fun2 :: Ref Bool -> Ref Bool -> Exp Bool -> SSM ()
fun2 = box "fun2" ["ref1","ref2","var3"] $ \ref1 ref2 var3 -> do
    ref1 <~ false'
    ref1 <~ (((negate 1) :: Exp Int) <. (0 - 0))
    if' (true' ==. var3)
      (return ())
      (Just (return ()))

fun3 :: Ref Bool -> Exp Bool -> SSM ()
fun3 = box "fun3" ["ref1","var2"] $ \ref1 var2 -> do
    if' ((((negate 2) - (negate 3)) :: Exp Int) <. (0 * (negate 2)))
      (do v0 <- deref ref1
          return ())
      (Just (wait [ref1]))
    v1 <- var (negate ((negate 2) - (negate 3)))
    wait [v1]