{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Fib where

import BinderAnn.Monadic
import SSM

mywait :: Ref Int -> SSM ()
mywait = box "mywait" ["r"] $ \r -> do
    wait [r]

mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" ["r1", "r2", "r"] $ \r1 r2 r -> do
    waitAll [r1,r2]
    v1 <- deref r1
    v2 <- deref r2
    after 1 r (v1 + v2)

myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" ["n", "r"] $ \n r -> do
    r1 <- var 0 
    r2 <- var 0
    if' (n <. 2)
            (after 1 r 1)
            (Just (fork [ myfib (n - 1) r1
                        , myfib (n - 2) r2
                        , mysum r1 r2 r
                        ]))