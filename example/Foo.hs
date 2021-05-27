{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Foo where

import BinderAnn.Monadic

import SSM
import Data.Int

foo :: Exp Bool -> Ref Int32 -> SSM ()
foo = box "foo" ["a", "b"] $ \a b -> do
    fork [ foo a b
         , foo (changed b) b
         ]