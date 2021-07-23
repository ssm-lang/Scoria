{-| This module exposes the interface a programmer is meant to use when writing
SSM programs. This is probably subject to extensive change as we move forward.

Right now there is no way to write polymorphic SSM procedures, if that procedure
needs to do anything other than waiting on its input references. There's an open
issue on GitHub where we are discussing monomorphisation strategies.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module SSM.Frontend.Language
    ( -- * The SSM Embedded language

      -- ** References
      {- | References in the language are parameterised over the type they reference.
      
      References can be aquired in 2 different ways. Either your SSM procedure
      receives a reference as an argument, in which case the reference is guaranteed
      to be alive for at least as long as your procedure, or you can create a new
      reference by using `var`. References created using `var` will be deallocated
      once the current process terminates. -}
      Ref
    , inputref
    , var
    , deref

      -- ** Expressions
      {- | Expressions are parameterised over the type they have.
      
      If the type variable @a@ has a `Num` instance and a `SSMType` instance, @Exp a@
      also has a `Num` instance. Expressions are also subjected to less than comparisons
      and equality checks. -}
    , Exp
    , (<.)
    , (==.)
    , neg

      -- *** Literals
      {- | The `Num` instance will in many cases figure out what type your literal should
      have, but if that is not the case, these functions can be used to explicitly create
      a literal of a concrete type.-}
    , int32
    , int64
    , uint8
    , uint64
    , true'
    , false'
    , event'

      -- ** Primitive statements
      {- | These are the primitive statements of the SSM language. Your procedure
      body is constructed by using these functions. The language is sequential, so the
      order in which you call these functions matters, naturally. -}
    , (<~)
    , wait
    , after
    , fork
    , changed
    , ifThen
    , ifThenElse
    , while'

      -- ** Derived statements
      -- | These are statements that are derived from the language primitives.
    , waitAll

    ) where

import Data.Int
import Data.Word
import Data.Proxy

import Control.Monad.State

import BinderAnn.Monadic

import SSM.Frontend.Syntax
import SSM.Frontend.Box

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
    abs _    = undefined
    signum _ = undefined

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg _ [] _              = error "No more parameter names"
    arg name (x:xs) (Exp b) = do
        emit $ Argument name x (Left b)
        return $ (Exp (Var (expType b) x), xs)

instance Arg (Ref a) where
    arg _ [] _                  = error "No more parameter names"
    arg name (x:xs) (Ptr (e,t)) = do
        emit $ Argument name x (Right (e,t))
        return (Ptr (x, t), xs)

-- | When interpreting or compiling a SSM program that requires input references,
-- supply this value instead.
inputref :: forall a. SSMType a => Ref a
inputref = Ptr ("dummy", Ref (typeOf (Proxy @a)))

class Assignable a b where
    -- | Immediate assignment
    (<~) :: a -> b -> SSM ()

instance Assignable (Exp a) (Exp a) where
    (Exp (Var t s)) <~ (Exp e) = emit $ SetLocal (Var t s) e
    e <~ _                     = error $ "can not assign a value to expression: " ++ show e

instance Assignable (Ref a) (Exp a) where
    (Ptr r) <~ (Exp e) = emit $ SetRef r e

-- | Less-than on numerical expressions
(<.) :: (Num a, SSMType a) => Exp a -> Exp a -> Exp Bool
Exp e1 <. Exp e2  = Exp $ BOp TBool e1 e2 OLT

-- | Equality on expressions
(==.) :: SSMType a => Exp a -> Exp a -> Exp Bool
Exp e1 ==. Exp e2 = Exp $ BOp TBool e1 e2 OEQ

{- | Negation of numerical expressions (works for unsigned ones right now,
but this should be remedied down the road) -}
neg :: (Num a, SSMType a) => Exp a -> Exp a
neg e@(Exp e') = Exp $ UOpE (typeOf e) e' Neg

-- | Explicity create an @Exp Int32@
int32 :: Int32 -> Exp Int32
int32 i = Exp $ Lit TInt32 $ LInt32 i

-- | Explicity create an @Exp Int64@
int64 :: Int64 -> Exp Int64
int64 i = Exp $ Lit TInt64 $ LInt64 i

-- | Explicity create an @Exp Word64@
uint64 :: Word64 -> Exp Word64
uint64 i = Exp $ Lit TUInt64 $ LUInt64 i

-- | Explicity create an @Exp Word8@
uint8 :: Word8 -> Exp Word8
uint8 i = Exp $ Lit TUInt8 $ LUInt8 i

-- | Boolean literal @True@
true' :: Exp Bool
true' = Exp $ Lit TBool $ LBool True

-- | Boolean literal @False@
false' :: Exp Bool
false' = Exp $ Lit TBool $ LBool False

-- | Event literal @()@
event' :: Exp ()
event' = Exp $ Lit TEvent LEvent

-- | Dereference a reference and get an expression holding the result
deref :: Ref a -> SSM (Exp a)
deref (Ptr r) = do
    n <- fresh
    emit $ GetRef (Fresh n) r
    return $ Exp $ Var (dereference (snd r)) n

{- | Create a new, local reference. This reference is deallocated when the procedure
it was created in terminates. -}
var :: Exp a -> SSM (Ref a)
var (Exp e) = do
    n <- fresh
    emit $ NewRef (Fresh n) e
    return $ Ptr $ (n, mkReference (expType e))

-- | Block until any of the references in the input list are be written to.
wait :: [Ref a] -> SSM ()
wait r = emit $ Wait (map (\(Ptr r') -> r') r)

{- | Scheduled assignment. @after d r v@ means that after @a@ units of time, the
reference @r@ should receive the value @v@. -}
after :: Exp Word64 -> Ref a -> Exp a -> SSM ()
after (Exp e) (Ptr r) (Exp v) = emit $ After e r v

-- | Fork one or more procedures.
fork :: [SSM ()] -> SSM ()
fork procs = emit $ Fork procs

{- | @changed r@ returns @true'@ if the reference @r@ was written to in
the current instant. -}
changed :: Ref a -> Exp Bool
changed (Ptr r) = Exp $ UOpR TBool r Changed

-- | Conditional executing with a dangling else
if' :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
if' (Exp c) thn els = emit $ If c thn els

-- | If the condition is true, execute the code in the second argument.
ifThen :: Exp Bool -> SSM () -> SSM ()
ifThen c thn = if' c thn Nothing

{- | If the condition is @true'@, execute the code in the second argument. If it is
@false'@, execute the code in the third argument. -}
ifThenElse :: Exp Bool -> SSM () -> SSM () -> SSM ()
ifThenElse c thn els = if' c thn (Just els)

-- | While the condition is @true'@, execute the code in the second argument.
while' :: Exp Bool -> SSM () -> SSM ()
while' (Exp c) bdy = emit $ While c bdy

waitSingle :: Ref a -> SSM ()
waitSingle = box "waitSingle" ["r"] $ \r -> wait [r]

{- | Wait for _all_ of the references in the input list to be written to at least once
each. -}
waitAll :: [Ref a] -> SSM ()
waitAll refs = fork $ map waitSingle refs
