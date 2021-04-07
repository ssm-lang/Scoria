{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Frontend
     ( Ref(..)
     , Exp(..)
     , inputIntRef
     , inputBoolRef
     , (+.)
     , (-.)
     , (*.)
     , (<.)
     , (==.)
     , (<~)
     , neg
     , int
     , true'
     , false'
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

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import BinderAnn.Monadic

import Core

instance AnnotatedM SSM (Exp a) where
    annotateM ma info = do
        v       <- ma
        stmts   <- gets statements
        let stmt = last stmts
        let stmt' = renameStmt stmt info
        modify $ \st -> st { statements = init stmts ++ [stmt']}
        return $ renameExp v info

instance AnnotatedM SSM (Ref a) where
    annotateM ma info = do
        v <- ma
        stmts <- gets statements
        let stmt = last stmts
        let stmt' = renameStmt stmt info
        modify $ \st -> st { statements = init stmts ++ [stmt']}
        return $ renameRef v info

renameStmt :: SSMStm -> SrcInfo -> SSMStm
renameStmt s (Info Nothing _)               = s
renameStmt s (Info _ Nothing)               = s
renameStmt s (Info (Just n) (Just (f,x,y))) =
    let srcinfo = Captured (f,x,y) n
    in case s of
        NewRef n e  -> NewRef srcinfo e
        GetRef r n  -> GetRef r srcinfo
        Changed r n -> Changed r srcinfo
        _           -> s

renameExp :: Exp a -> SrcInfo -> Exp a
renameExp e (Info Nothing _) = e
renameExp e (Info _ Nothing) = e
renameExp e (Info (Just n) _) = case e of
    Exp (Var t _) -> Exp $ Var t n
    _             -> e

renameRef :: Ref a -> SrcInfo -> Ref a
renameRef e (Info Nothing _) = e
renameRef e (Info _ Nothing) = e
renameRef (Ptr (_,t)) (Info (Just n) _) = Ptr $ (n, t)

newtype Ref a = Ptr Reference -- references that are shared, (variable name, ref to value)
  deriving Show
newtype Exp a = Exp SSMExp                 -- expressions
  deriving Show

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg name (x:xs) (Exp b) = do
        emit $ Argument name x (Left b)
        return $ (Exp (Var (expType b) x), xs)

instance Arg (Ref a) where
    arg name (x:xs) (Ptr (e,t)) = do
        emit $ Argument name x (Right (e,t))
        return (Ptr (x, t), xs)

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = emit $ Result name

inputIntRef :: Ref Int
inputIntRef = Ptr ("dummyintref", Ref TInt)

inputBoolRef :: Ref Bool
inputBoolRef = Ptr ("dummyboolref", Ref TBool)

class Assignable a b where
    (<~) :: a -> b -> SSM ()

instance Assignable (Exp a) (Exp a) where
    (Exp (Var t s)) <~ (Exp e) = emit $ SetLocal (Var t s) e
    e <~ _                     = error $ "can not assign a value to expression: " ++ show e

instance Assignable (Ref a) (Exp a) where
    (Ptr r) <~ (Exp e) = emit $ SetRef r e

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

true' :: Exp Bool
true' = Exp $ Lit TBool $ LBool True

false' :: Exp Bool
false' = Exp $ Lit TBool $ LBool False

deref :: Ref a -> SSM (Exp a)
deref (Ptr r) = do
    n <- fresh
    emit $ GetRef r (Fresh n)
    return $ Exp $ Var (dereference (snd r)) n

var :: Exp a -> SSM (Ref a)
var (Exp e) = do
    n <- fresh
    emit $ NewRef (Fresh n) e
    return $ Ptr $ (n, mkReference (expType e))

wait :: [Ref a] -> SSM ()
wait r = emit $ Wait (map (\(Ptr r') -> r') r)

-- | Delayed assignment
after :: Exp Int -> Ref a -> Exp a -> SSM ()
after (Exp e) (Ptr r) (Exp v) = emit $ After e r v

fork :: [SSM ()] -> SSM ()
fork procs = emit $ Fork procs

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed (Ptr r) = do
    n <- fresh
    emit $ Changed r (Fresh n)
    return $ Exp $ Var TBool n

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' (Exp c) thn els = emit $ If c thn els

while' :: Exp Bool -> SSM () -> SSM ()
while' (Exp c) bdy = emit $ While c bdy