{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}

import BinderAnn.Monadic
import SSM
import Ssm.Compiler.Cli(compileCli)

main :: IO ()
main = compileCli $ nonterminateNullary

nonterminateNullary :: SSM ()
nonterminateNullary = boxNullary "nonterminateNullary" $ do
    v <- var true'
    v <~ false'
    fork [ nonterminateNullary ]
