{-# LANGUAGE FlexibleInstances #-}
module SSM where

import AST

import Data.IORef
import Data.Maybe



type Ref a = (String, IORef SSMExp) -- references that are shared, (variable name, ref to value)
type Exp a = SSMExp                 -- expressions

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name b = Argument name (Left b) return >> return b

instance {-# OVERLAPPING #-} Arg (Ref a) where
    arg name e = Argument name (Right e) return >> return e

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () return >> return ()

(+.) :: Exp a -> Exp a -> Exp a
e1 +. e2  = BOp e1 e2 OPlus

(-.) :: Exp a -> Exp a -> Exp a
e1 -. e2  = BOp e1 e2 OMinus

(*.) :: Exp a -> Exp a -> Exp a
e1 *. e2  = BOp e1 e2 OTimes

(<.) :: Exp a -> Exp a -> Exp Bool
e1 <. e2  = BOp e1 e2 OLT

(==.) :: Exp a -> Exp a -> Exp Bool
e1 ==. e2 = BOp e1 e2 OEQ

int :: Int -> Exp Int
int i = Lit $ LInt i

deref :: Ref a -> SSM (Exp a)
deref r = GetRef r return

var :: String -> Exp a -> SSM (Ref a)
var n e = NewRef n e return

wait :: [Ref a] -> SSM ()
wait r = Wait r return

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after e r v = After e r v return

fork :: [SSM ()] -> SSM ()
fork procs = Fork procs return

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed r = Changed r return

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' c thn els = If c thn els return

while' :: Exp Bool -> SSM () -> SSM ()
while' c bdy = While c bdy return

-- fibonacci example

mywait :: Ref Int -> SSM ()
mywait = box "mywait" $ \r -> do
    wait [r]

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