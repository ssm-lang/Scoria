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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
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
    , (/=.)
    , (<<.)
    , (>>.)
    , (.&.)
    , (.|.)
    , xor
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
    , u32
    , u64
    , min'
    , max'
    , true'
    , false'
    , event'

    --- *** Time wrappers
    , Time
    , nsecs
    , usecs
    , msecs
    , secs
    , mins
    , hrs
    , time2ns

      -- ** Primitive statements
      {- | These are the primitive statements of the SSM language. Your procedure
      body is constructed by using these functions. The language is sequential, so the
      order in which you call these functions matters, naturally. -}
    , (<~)
    , assign
    , wait
    , after
    , fork
    , changed
    , unchanged
    , ifThen
    , ifThenElse
    , while
    , doWhile

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
import SSM.Frontend.Ref
import SSM.Frontend.Exp
import SSM.Frontend.Compile
import SSM.Frontend.Waitable

-- | When interpreting or compiling a SSM program that requires input references,
-- supply this value instead.
inputref :: forall a. SSMType a => Ref a
inputref = Ptr $ makeDynamicRef (Ident "dummy" Nothing) (Ref (typeOf (Proxy @a)))

{- | Class of types @a@ and @b@ where we can perform an immediate assignment of an @b@
to an @a@. -}
class Assignable a b | a -> b where
    -- | Immediate assignment
    (<~) :: a -> b -> SSM ()

-- | We can assign expressions to expressions, if that expression is a variable.
instance Assignable (Exp a) (Exp a) where
    (Exp (Var t s)) <~ (Exp e) = emit $ SetLocal (Var t s) e
    e <~ _                     = error $ "can not assign a value to expression: " ++ show e

-- | We can always assign an expression to a reference.
instance Assignable (Ref a) (Exp a) where
    (<~) = assign

infixl 1 <~

-- | Assigning values to references
assign :: Ref a -> Exp a -> SSM ()
assign (Ptr r) (Exp e) = emit $ SetRef r e

infix 4 <.
-- | Less-than on numerical expressions
Exp e1 <. Exp e2  = Exp $ BOp TBool e1 e2 OLT
(<.) :: (Num a, SSMType a) => Exp a -> Exp a -> Exp Bool

infix 4 ==.
-- | Equality on expressions (level 4)
(==.) :: SSMType a => Exp a -> Exp a -> Exp Bool
Exp e1 ==. Exp e2 = Exp $ BOp TBool e1 e2 OEQ

infix 4 /=.
(/=.) :: SSMType a => Exp a -> Exp a -> Exp Bool
e1 /=. e2 = not' $ e1 ==. e2

infixl 8 <<.
(<<.) :: (SSMType a, Eq a) => Exp a -> Exp a -> Exp a
e@(Exp e1) <<. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OLShift

infixl 8 >>.
(>>.) :: (SSMType a, Eq a) => Exp a -> Exp a -> Exp a
e@(Exp e1) >>. Exp e2 = Exp $ BOp (typeOf e) e1 e2 ORShift

infixl 7 .&.
(.&.) :: (SSMType a, Eq a) => Exp a -> Exp a -> Exp a
e@(Exp e1) .&. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OBAnd

infixl 5 .|.
(.|.) :: (SSMType a, Eq a) => Exp a -> Exp a -> Exp a
e@(Exp e1) .|. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OBOr

xor :: (SSMType a, Eq a) => Exp a -> Exp a -> Exp a
xor e@(Exp e1) (Exp e2) = Exp $ BOp (typeOf e) e1 e2 OBXor

infixr 3 &&.
-- | Boolean conjunction
(&&.) :: Exp Bool -> Exp Bool -> Exp Bool
e@(Exp e1) &&. Exp e2 = Exp $ BOp (typeOf e) e1 e2 OAnd

infixr 2 ||.
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

-- | Explicity create an @Exp Word32@
u32 :: Word32 -> Exp Word32
u32 i = Exp $ Lit TUInt32 $ LUInt32 i

-- | Explicity create an @Exp Word8@
u8 :: Word8 -> Exp Word8
u8 i = Exp $ Lit TUInt8 $ LUInt8 i

-- | Minimum of two values
min' :: forall a . (SSMType a, Num a) => Exp a -> Exp a -> Exp a
min' (Exp e1) (Exp e2) = Exp $ BOp (typeOf (Proxy @a)) e1 e2 OMin

-- | Maximum of two values
max' :: forall a . (SSMType a, Num a) => Exp a -> Exp a -> Exp a
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

{- | Abstract type that's used to talk about times. The implementation details should
not be relied upon, and the strict API exposed here should be used.

@Exp Time@ can be created with:

  1. Numeric literals (which denote time in milliseconds)
  2. nsecs
  3. usecs
  4. msecs
  5. secs
  6. mins
  7. hrs

-}
newtype Time = Time Word64 -- ^ We only use @Time@ as a Phantom type though
  deriving Num      via Word64 -- but we derive these goodies to get type information
  deriving Ord      via Word64
  deriving Eq       via Word64
  deriving Real     via Word64
  deriving Enum     via Word64
  deriving Integral via Word64

instance SSMType Time where
  typeOf _ = TUInt64

instance {-# OVERLAPPING #-} Num (Exp Time) where
  Exp e1 + Exp e2 = Exp $ BOp TUInt64 e1 e2 OPlus
  Exp e1 - Exp e2 = Exp $ BOp TUInt64 e1 e2 OMinus
  Exp e1 * Exp e2 = Exp $ BOp TUInt64 e1 e2 OTimes
  fromInteger i = msecs $ fromInteger i
  abs e = e
  signum _ = Exp $ Lit TUInt64 $ LUInt64 1

-- | Specify @e@ has units of nanoseconds. 
nsecs :: Exp Word64 -> Exp Time
nsecs (Exp e) = Exp e

-- | Specify @e@ has units of microseconds. 
usecs :: Exp Word64 -> Exp Time
usecs (Exp e) = Exp $ BOp TUInt64 e factor OTimes
  where
    factor :: SSMExp
    factor = Lit TUInt64 $ LUInt64 1000

-- | Specify @e@ has units of milliseconds. 
msecs :: Exp Word64 -> Exp Time
msecs (Exp e) = Exp $ BOp TUInt64 e factor OTimes
  where
    factor :: SSMExp
    factor = Lit TUInt64 $ LUInt64 1000000

-- | Specify @e@ has units of seconds. 
secs :: Exp Word64 -> Exp Time
secs (Exp e) = Exp $ BOp TUInt64 e factor OTimes
  where
    factor :: SSMExp
    factor = Lit TUInt64 $ LUInt64 1000000000

-- | Specify @e@ has units of minutes. 
mins :: Exp Word64 -> Exp Time
mins (Exp e) = Exp $ BOp TUInt64 e factor OTimes
  where
    factor :: SSMExp
    factor = Lit TUInt64 $ LUInt64 60000000000

-- | Specify @e@ has units of hours.
hrs :: Exp Word64 -> Exp Time
hrs (Exp e) = Exp $ BOp TUInt64 e factor OTimes 
  where
    factor :: SSMExp
    factor = Lit TUInt64 $ LUInt64 3600000000000

{- | Turn a `SSMTime` into a `Exp Word64`, where the @Word64@ denotes the time in
nanoseconds. -}
time2ns :: Exp Time -> Exp Word64
time2ns (Exp e) = Exp e

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

-- | Generate waitable instances for different sized tuples
$(return $ map makeWaitableInstance [1..62])

instance Waitable (Ref a) where
  wait (Ptr r) = emit $ Wait [r]

-- | Wait for a reference to assume a specific value
waitFor :: SSMType a => Ref a -> Exp a -> SSM ()
waitFor r e = do
  doWhile (wait r) (deref r ==. e)

{- | Scheduled assignment. @after d r v@ means that after @d@ units of time, the
reference @r@ should receive the value @v@. -}
after :: Exp Time -> Ref a -> Exp a -> SSM ()
after (Exp d) (Ptr r) (Exp v) = emit $ After d r v

-- | Fork one or more procedures.
fork :: [SSM ()] -> SSM ()
fork procs = emit $ Fork procs

{- | @changed r@ returns @true'@ if the reference @r@ was written to in
the current instant. -}
changed :: Ref a -> Exp Bool
changed (Ptr r) = Exp $ UOpR TBool r Changed

{- | @unchanged@ returns @true'@ if the reference @r@ was not written to in
the current instant. It is the dual to @changed@, and is equivalent to
@not . changed@. -}
unchanged :: Ref a -> Exp Bool
unchanged = not' . changed

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
waitSingle = box "waitSingle" ["r"] $ \r -> wait r

{- | Wait for _all_ of the references in the input list to be written to at least once
each. -}
waitAll :: [Ref a] -> SSM ()
waitAll refs = fork $ map waitSingle refs
