{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}

import BinderAnn.Monadic
import SSM
import Ssm.Compiler.Cli(compileCli)

main :: IO ()
main = compileCli $ fun64 10 inputref

fun64 :: Exp Word64 -> Ref Word64 -> SSM ()
fun64 = box "fun64" ["e", "r"] $ \e r -> do
    v <- deref r
    after (v + e) r e
    wait [r]
    fork [ fun64 (e + 1) r ]
