{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}

import BinderAnn.Monadic
import SSM
import Ssm.Compiler.Cli(compileCli)

main :: IO ()
main = compileCli $ foo True inputref

foo :: Exp Bool -> Ref Int32 -> SSM ()
foo = box "foo" ["a", "b"] $ \a b -> do
    fork [ foo a b
         , foo (changed b) b
         ]
