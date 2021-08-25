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

      -- ** Expressions
      {- | Expressions are parameterised over the type they have.
      
      If the type variable @a@ has a `Num` instance and a `SSMType` instance, @Exp a@
      also has a `Num` instance. Expressions are also subjected to less than comparisons
      and equality checks. -}
    , Exp
    , (<.)
    , (==.)
    , (&&.)
    , (||.)
    , not'
    , (/.)
    , (%.)
    , neg
    , deref

      -- *** Literals
      {- | The `Num` instance will in many cases figure out what type your literal should
      have, but if that is not the case, these functions can be used to explicitly create
      a literal of a concrete type.-}
    , i32
    , i64
    , u8
    , u64
    , min'
    , max'
    , true'
    , false'
    , event'

    --- *** Time wrappers
    , SSMTime
    , nsecs
    , usecs
    , msecs
    , secs
    , mins
    , hrs
    , (//)

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
    , while
    , doWhile

      -- ** Derived statements
      -- | These are statements that are derived from the language primitives.
    , waitAll

      -- ** Global references
      -- | Global references exist in the global scope and are always alive.
    , global
    ) where

import Data.Int
import Data.Word
import Data.Proxy

import Control.Monad.State

import BinderAnn.Monadic

import SSM.Frontend.Syntax
import SSM.Frontend.Box
import SSM.Frontend.Ref
import SSM.Frontend.Exp
import SSM.Frontend.Compile

-- | When interpreting or compiling a SSM program that requires input references,
-- supply this value instead.
inputref :: forall a. SSMType a => Ref a
inputref = Ptr $ makeDynamicRef (Ident "dummy" Nothing) (Ref (typeOf (Proxy @a)))

{- | Class of types @a@ and @b@ where we can perform an immediate assignment of an @b@
to an @a@. -}
class Assignable a b where
    -- | Immediate assignment
    (<~) :: a -> b -> SSM ()

-- | We can assign expressions to expressions, if that expression is a variable.
instance Assignable (Exp a) (Exp a) where
    (Exp (Var t s)) <~ (Exp e) = emit $ SetLocal (Var t s) e
    e <~ _                     = error $ "can not assign a value to expression: " ++ show e

-- | We can always assign an expression to a reference.
instance Assignable (Ref a) (Exp a) where
    (Ptr r) <~ (Exp e) = emit $ SetRef r e

-- | Less-than on numerical expressions
(<.) :: (Num a, SSMType a) => Exp a -> Exp a -> Exp Bool
Exp e1 <. Exp e2  = Exp $ BOp TBool e1 e2 OLT

-- | Equality on expressions
(==.) :: SSMType a => Exp a -> Exp a -> Exp Bool
Exp e1 ==. Exp e2 = Exp $ BOp TBool e1 e2 OEQ

-- | Boolean conjunction
(&&.) :: Exp Bool -> Exp Bool -> Exp Bool
e@(Exp e1) &&. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OAnd

-- | Boolean disjunction
(||.) :: Exp Bool -> Exp Bool -> Exp Bool
e@(Exp e1) ||. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OOr

-- | Division of expressions
(/.) :: forall a . (SSMType a, Integral a) => Exp a -> Exp a -> Exp a
Exp e1 /. Exp e2 = Exp $ BOp (typeOf (Proxy @a)) e1 e2 ODiv

-- | Modulus
(%.) :: (SSMType a, Integral a) => Exp a -> Exp a -> Exp a
e@(Exp e1) %. Exp e2 = Exp $ BOp (typeOf e) e1 e2 ORem

{- | Negation of numerical expressions (works for unsigned ones right now,
but this should be remedied down the road) -}
neg :: (Num a, SSMType a) => Exp a -> Exp a
neg e@(Exp e') = Exp $ UOpE (typeOf e) e' Neg

-- | Boolean negation
not' :: Exp Bool -> Exp Bool
not' e@(Exp e') = Exp $ UOpE (typeOf e) e' Not

-- | Explicity create an @Exp Int32@
i32 :: Int32 -> Exp Int32
i32 i = Exp $ Lit TInt32 $ LInt32 i

-- | Explicity create an @Exp Int64@
i64 :: Int64 -> Exp Int64
i64 i = Exp $ Lit TInt64 $ LInt64 i

-- | Explicity create an @Exp Word64@
u64 :: Word64 -> Exp Word64
u64 i = Exp $ Lit TUInt64 $ LUInt64 i

-- | Explicity create an @Exp Word8@
u8 :: Word8 -> Exp Word8
u8 i = Exp $ Lit TUInt8 $ LUInt8 i

min' :: forall a . SSMType a => Exp a -> Exp a -> Exp a
min' (Exp e1) (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OMin

max' :: forall a . SSMType a => Exp a -> Exp a -> Exp a
max' (Exp e1) (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OMax

-- | Boolean literal @True@
true' :: Exp Bool
true' = Exp $ Lit TBool $ LBool True

-- | Boolean literal @False@
false' :: Exp Bool
false' = Exp $ Lit TBool $ LBool False

-- | Event literal @()@
event' :: Exp ()
event' = Exp $ Lit TEvent LEvent

-- Time wrappers for use in stmts/expressions that expect time values.

-- | Specify @e@ has units of nanoseconds. 
nsecs :: Exp Word64 -> SSMTime
nsecs (Exp e) = SSMTime e SSMNanosecond

-- | Specify @e@ has units of microseconds. 
usecs :: Exp Word64 -> SSMTime
usecs (Exp e) = SSMTime e SSMMicrosecond

-- | Specify @e@ has units of milliseconds. 
msecs :: Exp Word64 -> SSMTime
msecs (Exp e) = SSMTime e SSMMillisecond

-- | Specify @e@ has units of seconds. 
secs :: Exp Word64 -> SSMTime
secs (Exp e) = SSMTime e SSMSecond

-- | Specify @e@ has units of minutes. 
mins :: Exp Word64 -> SSMTime
mins (Exp e) = SSMTime e SSMMinute

-- | Specify @e@ has units of hours.
hrs :: Exp Word64 -> SSMTime
hrs (Exp e) = SSMTime e SSMHour

{- | More direct division of time. E.g @t // 5@ divides a time into one fifth of the
original time. This function can be convenient when you need to calculate delays etc. -}
(//) :: SSMTime -> Exp Word64 -> SSMTime
t // (Exp d) = SSMTimeDiv t d

instance Num SSMTime where
    t1 + t2       = SSMTimeAdd t1 t2
    t1 - t2       = SSMTimeSub t1 t2
    t1 * t2       = undefined
    fromInteger _ = undefined
    abs _         = undefined
    signum _      = undefined

-- | Dereference a reference and get an expression holding the result
deref :: forall a . SSMType a => Ref a -> Exp a
deref (Ptr r) = Exp $ UOpR (typeOf (Proxy @a)) r Deref

{- | Create a new, local reference. This reference is deallocated when the procedure
it was created in terminates. -}
var :: Exp a -> SSM (Ref a)
var (Exp e) = do
    n <- fresh
    let id = Ident n Nothing
    emit $ NewRef id e
    return $ Ptr $ makeDynamicRef id (mkReference $ expType e)

-- | Block until any of the references in the input list are be written to.
wait :: [Ref a] -> SSM ()
wait r = emit $ Wait (map (\(Ptr r') -> r') r)

waitFor :: SSMType a => Ref a -> Exp a -> SSM ()
waitFor r e = do
  doWhile (wait [r]) (deref r ==. e)

{- | Scheduled assignment. @after d r v@ means that after @d@ units of time, the
reference @r@ should receive the value @v@. -}
after :: SSMTime -> Ref a -> Exp a -> SSM ()
after d (Ptr r) (Exp v) = emit $ After d r v

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
while :: Exp Bool -> SSM () -> SSM ()
while (Exp c) bdy = emit $ While c bdy

doWhile :: SSM () -> Exp Bool -> SSM ()
doWhile bdy c = bdy >> while c bdy

waitSingle :: Ref a -> SSM ()
waitSingle = box "waitSingle" ["r"] $ \r -> wait [r]

{- | Wait for _all_ of the references in the input list to be written to at least once
each. -}
waitAll :: [Ref a] -> SSM ()
waitAll refs = fork $ map waitSingle refs






-- | Create a global reference
global :: forall a . SSMType a => Compile (Ref a)
global = do
    n <- fresh
    let id = Ident n Nothing
    let t = mkReference $ typeOf $ Proxy @a
    addGlobal id t
    return $ Ptr $ makeStaticRef id t

{- | If BinderAdd is enabled, we can grab the names declared in the source code instead
of generating fresh named. -}
instance AnnotatedM Compile (Ref a) where
    annotateM ma info = do
        -- get state before running the action
        st1 <- get
        ref <- ma
        -- get state after running the action
        st2 <- get

        if st1 `hasSameGlobalsAs` st2
            {- If ma didn't actually declare a new reference, just return the one the
            action ma already returned. -}
            then return ref
            -- Otherwise, rename if with the source information, if any exist
            else do SSM.Frontend.Language.renameNewestGlobal info
                    return $ SSM.Frontend.Ref.renameRef ref info

{- | Rename the newest global reference according to the source information found
in the first argument. -}
renameNewestGlobal :: SrcInfo -> Compile ()
renameNewestGlobal (Info (Just n) l) = SSM.Frontend.Compile.renameNewestGlobal (Ident n l)
renameNewestGlobal _                 = return ()
