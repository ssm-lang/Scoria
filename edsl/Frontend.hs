{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Frontend
     ( Ref(..)
     , Exp(..)
     , Lit(..)
     , inputref
     , (+.)
     , (-.)
     , (*.)
     , (<.)
     , (==.)
     , (<~)
     , neg
     , int32
     , int64
     , uint64
     , word8
     , true'
     , false'
     , deref
     , var
     , wait
     , waitAll
     , after
     , fork
     , changed
     , if'
     , ifThen
     , ifThenElse
     , while'
     , SSM -- reexport so we don't need to import Core and get all the constructors
     , Box(..)
     , Int64(..)
     , Word64(..)
) where

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import BinderAnn.Monadic

import Data.Int
import Data.Word
import Data.Proxy

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
        GetRef n r  -> GetRef srcinfo r
        Changed n r -> Changed srcinfo r
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
newtype Lit a = FLit SSMLit
  deriving Show

class FromLiteral a where
    fromLit :: a -> Lit a

instance FromLiteral Int32 where
    fromLit i = FLit $ LInt32 (fromIntegral i)

instance FromLiteral Int64 where
    fromLit i = FLit $ LInt64 (fromIntegral i)

instance FromLiteral Word64 where
    fromLit i = FLit $ LUInt64 (fromIntegral i)

instance FromLiteral Word8 where
    fromLit i = FLit $ LUInt8 (fromIntegral i)

instance (Num a, FromLiteral a, SSMType a) => Num (Exp a) where
    (Exp e1) + (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OPlus
    (Exp e1) - (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OMinus
    (Exp e1) * (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OTimes
    fromInteger i = let FLit l = fromLit (fromInteger @a i)
                    in Exp $ Lit (typeOf (Proxy @a)) l

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

-- | When interpreting or compiling a SSM program that requires input references,
-- supply this value instead.
inputref :: forall a. SSMType a => Ref a
inputref = Ptr ("dummy", Ref (typeOf (Proxy @a)))

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

neg :: (Num a, SSMType a) => Exp a -> Exp a
neg e@(Exp e') = Exp $ UOp (typeOf e) e' Neg

int32 :: Int32 -> Exp Int32
int32 i = Exp $ Lit TInt32 $ LInt32 i

int64 :: Int64 -> Exp Int64
int64 i = Exp $ Lit TInt64 $ LInt64 i

uint64 :: Word64 -> Exp Word64
uint64 i = Exp $ Lit TUInt64 $ LUInt64 i

word8 :: Word8 -> Exp Word8
word8 i = Exp $ Lit TUInt8 $ LUInt8 i

true' :: Exp Bool
true' = Exp $ Lit TBool $ LBool True

false' :: Exp Bool
false' = Exp $ Lit TBool $ LBool False

deref :: Ref a -> SSM (Exp a)
deref (Ptr r) = do
    n <- fresh
    emit $ GetRef (Fresh n) r
    return $ Exp $ Var (dereference (snd r)) n

var :: Exp a -> SSM (Ref a)
var (Exp e) = do
    n <- fresh
    emit $ NewRef (Fresh n) e
    return $ Ptr $ (n, mkReference (expType e))

wait :: [Ref a] -> SSM ()
wait r = emit $ Wait (map (\(Ptr r') -> r') r)

-- | Delayed assignment
after :: Exp Word64 -> Ref a -> Exp a -> SSM ()
after (Exp e) (Ptr r) (Exp v) = emit $ After e r v

fork :: [SSM ()] -> SSM ()
fork procs = emit $ Fork procs

-- The @-operator
changed :: Ref a -> SSM (Exp Bool)
changed (Ptr r) = do
    n <- fresh
    emit $ Changed (Fresh n) r
    return $ Exp $ Var TBool n

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' (Exp c) thn els = emit $ If c thn els

ifThen :: Exp Bool -> SSM () -> SSM ()
ifThen c thn = if' c thn Nothing

ifThenElse :: Exp Bool -> SSM () -> SSM () -> SSM ()
ifThenElse c thn els = if' c thn (Just els)

while' :: Exp Bool -> SSM () -> SSM ()
while' (Exp c) bdy = emit $ While c bdy

waitSingle :: Ref a -> SSM ()
waitSingle = box "waitSingle" ["r"] $ \r -> wait [r]

waitAll :: [Ref a] -> SSM ()
waitAll refs = fork $ map waitSingle refs

{-
limit :: Exp Int64
limit = undefined

after64 :: Exp Int64 -> Ref a -> Exp a -> SSM ()
after64 = box "after64" ["delay","r","v"] $ \delay r v -> do
    r <- var 0

    while' (delay <. limit) $ do
      after limit r 1
      wait [r]
      delay <~ (delay - limit)
    
    after delay r v

unsafeForkAndContinue :: SSM () -> SSM ()
unsafeForkAndContinue ssm = undefined
-}
