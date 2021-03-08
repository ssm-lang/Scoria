{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Foo where

import BinderAnn.Monadic

import Frontend

foo :: Exp Bool -> Ref Int -> SSM ()
foo = box "foo" ["a", "b"] $ \a b -> do
    c <- changed b
    fork [ foo a b
         , foo c b
         ]