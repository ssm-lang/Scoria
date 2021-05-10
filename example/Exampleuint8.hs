{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Exampleuint8 where

import BinderAnn.Monadic
import Frontend
import Data.Word

test8bit :: Exp Word8 -> Ref Word8 -> SSM ()
test8bit = box "test8bit" ["e","r"] $ \e r -> do
    v <- deref r
    after (uint64 5) r (v + e)
    fork [ test8bit (e + (word8 1)) r ]