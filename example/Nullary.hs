{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Nullary where

import BinderAnn.Monadic
import SSM
import Data.Int

nonterminateNullary :: SSM ()
nonterminateNullary = boxNullary "nonterminateNullary" $ do
    v <- var true'
    v <~ false'
    fork [ nonterminateNullary ]