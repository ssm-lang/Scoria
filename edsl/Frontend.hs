{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Frontend
     ( Ref(..)
     , Exp(..)
     , (+.)
     , (-.)
     , (*.)
     , (<.)
     , (==.)
     , (<~)
     , neg
     , int
     , deref
     , var
     , wait
     , after
     , fork
     , changed
     , if'
     , while'
     , SSM -- reexport so we don't need to import Core and get all the constructors
     , Box(..)
) where

import Core

type Ref a = Reference -- references that are shared, (variable name, ref to value)
type Exp a = SSMExp                 -- expressions

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name (x:xs) b = Argument name x (Left b) return >> return (Var x, xs)

instance Arg (Ref a) where
    arg name (x:xs) e = Argument name x (Right e) return >> return (x, xs)

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () return >> return ()

class Assignable a where
    (<~) :: a -> SSMExp -> SSM ()

instance Assignable (Exp a) where
    (Var s) <~ e = SetLocal (Var s) e return
    e <~ _       = error $ "can not assign a value to expression: " ++ show e

instance Assignable (Ref a) where
    r <~ e = SetRef r e return

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

neg :: Exp a -> Exp a
neg e = UOp e Neg

int :: Int -> Exp Int
int i = Lit $ LInt i

deref :: Ref a -> SSM (Exp a)
deref r = GetRef r Nothing return

var :: Exp a -> SSM (Ref a)
var e = NewRef Nothing e return

wait :: [Ref a] -> SSM ()
wait r = Wait r return

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after e r v = After e r v return

fork :: [SSM ()] -> SSM ()
fork procs = Fork procs return

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed r = Changed r Nothing return

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' c thn els = If c thn els return

while' :: Exp Bool -> SSM () -> SSM ()
while' c bdy = While c bdy return