module Client where

import AST
import SSM

{-

If we have this:
mysum r1 r2 r = do
    body

We already have all the information for the modified binderadd plugin to transform this into
mysum = box "mysum" $ \r1 r2 r -> do
    body

I had a meeting with Agustin where he pointed out that we could just supply the
variable names to the box function?
mysum = box "mysum" ["r1","r2","r"] $ \r1 r2 r -> do
    body
-}

mywait :: Ref Int -> SSM ()
mywait = box "mywait" $ \r -> do
    wait [r]

{-mysum r1 r2 r = do
    fork (mywait r1) (mywait r2)
    v1 <- deref r1
    v2 <- deref r2
    after 1 r (r1 + r2)-}

mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" $ \r1 r2 r -> do
    fork [ mywait r1
         , mywait r2
         ]
    v1 <- deref r1
    v2 <- deref r2
    after (int 1) r (v1 +. v2)

myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" $ \n r -> do
    r1 <- var "r1" (int 0)
    r2 <- var "r2" (int 0)
    if' (n <. (int 2))
            (after (int 1) r (int 1))
            (Just (fork [ myfib (n -. int 1) r1
                        , myfib (n -. int 2) r2
                        , mysum r1 r2 r
                        ]))

mymain :: SSM ()
mymain = var "r" (int 0) >>= \r -> fork [myfib (int 13) r]

test :: Exp Int -> SSM ()
test = box "test" $ \v ->
    if' (v <. (int 2))
      (fork [test v])
      (Just (fork [test v]))