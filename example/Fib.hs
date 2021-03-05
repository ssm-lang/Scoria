module Fib where

import Core
import Frontend

mywait :: Ref Int -> SSM ()
mywait = box "mywait" ["r"] $ \r -> do
    wait [r]

mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" ["r1", "r2", "r"] $ \r1 r2 r -> do
    fork [ mywait r1
         , mywait r2
         ]
    v1 <- deref r1
    v2 <- deref r2
    after (int 1) r (v1 +. v2)

myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" ["n", "r"] $ \n r -> do
    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. int 2)
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) r1
                        , myfib (n -. int 2) r2
                        , mysum r1 r2 r
                        ]))