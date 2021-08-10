{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Exampleuint8 where

import BinderAnn.Monadic
import SSM
import Ssm.Compiler.Cli(compileCli)

main :: IO ()
main = compileCli $ test8bit 24 inputref

test8bit :: Exp Word8 -> Ref Word8 -> SSM ()
test8bit = box "test8bit" ["e","r"] $ \e r -> do
    v <- deref r
    after (uint64 5) r (v + e)
    fork [ test8bit (e + (word8 1)) r ]
