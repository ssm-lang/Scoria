{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- (runCGen (examp (Ptr ("a", Ref TInt)))) >>= putStrLn

newtype Ref a = Ptr Reference -- references that are shared, (variable name, ref to value)
  deriving Show
newtype Exp a = Exp SSMExp                 -- expressions
  deriving Show

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name (x:xs) (Exp b) = Argument name x (Left b) return >> return (Exp (Var undefined x), xs)

instance Arg (Ref a) where
    arg name (x:xs) (Ptr (e,t)) = Argument name x (Right (e,t)) return >> return (Ptr (x, t), xs)

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = Result name () return >> return ()

--instance SSMType a => Typeable (Exp a) where
--    typeOf _ = fetchType (Proxy @a)

--instance SSMType a => Typeable (Ref a) where
--    typeOf _ = undefined

class Assignable a b where
    (<~) :: a -> b -> SSM ()

instance Assignable (Exp a) (Exp a) where
    (Exp (Var t s)) <~ (Exp e) = SetLocal (Var t s) e return
    e <~ _       = error $ "can not assign a value to expression: " ++ show e

instance Assignable (Ref a) (Exp a) where
    (Ptr r) <~ (Exp e) = SetRef r e return

(+.) :: SSMType a => Exp a -> Exp a -> Exp a
e@(Exp e1) +. Exp e2  = Exp $ BOp (typeOf e) e1 e2 OPlus

(-.) :: SSMType a => Exp a -> Exp a -> Exp a
e@(Exp e1) -. Exp e2  = Exp $ BOp (typeOf e) e1 e2 OMinus

(*.) :: SSMType a => Exp a -> Exp a -> Exp a
e@(Exp e1) *. Exp e2  = Exp $ BOp (typeOf e) e1 e2 OTimes

(<.) :: SSMType a => Exp a -> Exp a -> Exp Bool
Exp e1 <. Exp e2  = Exp $ BOp TBool e1 e2 OLT

(==.) :: SSMType a => Exp a -> Exp a -> Exp Bool
Exp e1 ==. Exp e2 = Exp $ BOp TBool e1 e2 OEQ

neg :: SSMType a => Exp a -> Exp a
neg e@(Exp e') = Exp $ UOp (typeOf e) e' Neg

int :: Int -> Exp Int
int i = Exp $ Lit TInt $ LInt i

deref :: Ref a -> SSM (Exp a)
deref (Ptr r) = Exp <$> GetRef r Nothing return

var :: Exp a -> SSM (Ref a)
var (Exp e) = Ptr <$> NewRef Nothing e return

wait :: [Ref a] -> SSM ()
wait r = Wait (map (\(Ptr r') -> r') r) return

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after (Exp e) (Ptr r) (Exp v) = After e r v return

fork :: [SSM ()] -> SSM ()
fork procs = Fork procs return

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed (Ptr r) = Exp <$> Changed r Nothing return

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' (Exp c) thn els = If c thn els return

while' :: Exp Bool -> SSM () -> SSM ()
while' (Exp c) bdy = While c bdy return