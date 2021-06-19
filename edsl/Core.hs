{-# LANGUAGE GADTs#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Core where

import Control.DeepSeq
import GHC.Generics

import Data.Int
import Data.Word
import qualified Data.Typeable as T

import Control.Monad.State.Lazy
import BinderAnn.Monadic

type Reference = (String, Type)

data Name = Fresh String
          | Captured (String,Int,Int) String
  deriving (Show, Eq, Read)

getVarName :: Name -> String
getVarName (Fresh n)      = n
getVarName (Captured _ n) = n

data SSMStm = -- | Variable/Stream operations
              NewRef Name SSMExp
            | GetRef Name Reference
            | SetRef Reference SSMExp
            | SetLocal SSMExp SSMExp

              -- | Control operations
            | If SSMExp (SSM ()) (Maybe (SSM ()))
            | While SSMExp (SSM ())
            
              -- | SSM specific operations
            | After SSMExp Reference SSMExp
            | Wait [Reference]
            | Fork [SSM ()]

              -- | Procedure construction
            | Procedure String
            | Argument String String (Either SSMExp Reference)
            | Result String

data SSMSt = SSMSt { counter    :: Int
                   , statements :: [SSMStm]
                   }

newtype SSM a = SSM (State SSMSt a)
  deriving Functor            via State SSMSt
  deriving Applicative        via State SSMSt
  deriving Monad              via State SSMSt
  deriving (MonadState SSMSt) via State SSMSt

runSSM :: SSM a -> [SSMStm]
runSSM (SSM program) = statements $ execState program (SSMSt 0 [])

pureSSM :: [SSMStm] -> SSM ()
pureSSM stmts = modify $ \st -> st { statements = stmts }

emit :: SSMStm -> SSM ()
emit stm = modify $ \st -> st { statements = statements st ++ [stm]}

fresh :: SSM String
fresh = do
    i <- gets counter
    modify $ \st -> st { counter = i + 1 }
    return $ "v" ++ show i

getProcedureName :: SSMStm -> String
getProcedureName (Procedure n) = n
getProcedureName _             = error "not a procedure"

data Type = TInt32
          | TUInt8
          | TInt64
          | TUInt64
          | TBool
          | Special String
          | Ref Type
  deriving (Eq, Show, Generic, NFData, Read)

class SSMType a where
    typeOf :: proxy a -> Type

instance SSMType Int32 where
    typeOf _ = TInt32

instance SSMType Bool where
    typeOf _ = TBool

instance SSMType Word8 where
  typeOf _ = TUInt8

instance SSMType Int64 where
    typeOf _ = TInt64

instance SSMType Word64 where
    typeOf _ = TUInt64

expType :: SSMExp -> Type
expType (Var t _)     = t
expType (Lit t _)     = t
expType (UOpE t _ _)  = t
expType (UOpR t _ _)  = t
expType (BOp t _ _ _) = t

refType :: Reference -> Type
refType (_,t) = t

dereference :: Type -> Type
dereference (Ref t) = t
dereference t       = error $ "not a reference type: can not dereference " ++ show t

mkReference :: Type -> Type
mkReference = Ref

isReference :: Type -> Bool
isReference (Ref _) = True
isReference _       = False

-- | SSM expressions
data SSMExp = Var Type String               -- ^ Variables
            | Lit Type SSMLit               -- ^ Literals
            | UOpE Type SSMExp UnaryOpE     -- ^ Unary operators on expressions
            | UOpR Type Reference UnaryOpR  -- ^ Unary operators on references
            | BOp Type SSMExp SSMExp BinOp  -- ^ Binary operators
  deriving (Eq, Generic, NFData, Show, Read)

-- | SSM literals
data SSMLit = LInt32 Int32    -- ^ Integer literals
            | LUInt8 Word8    -- ^ 8bit unsigned integers
            | LInt64 Int64    -- ^ 64bit integer literals
            | LUInt64 Word64  -- ^ 64bit unsigned integer literals
            | LBool Bool      -- ^ Boolean literals
  deriving (Eq, Generic, NFData, Show, Read)

{-- | SSM unary operators on expressions -}
data UnaryOpE = Neg  -- ^ negation
  deriving (Show, Eq, Generic, NFData, Read)

{-| SSM unary operators on references -}
data UnaryOpR = Changed  -- ^ Expression represents if the reference has been written to
  deriving (Show, Eq, Generic, NFData, Read)

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp = OPlus   -- ^ addition
           | OMinus  -- ^ subtraction
           | OTimes  -- ^ multiplication
           | OLT     -- ^ less-than
           | OEQ     -- ^ eq
  deriving (Eq, Generic, NFData, Show, Read)

-- | Arguments to our functions
class Arg a where
    arg :: String -> [String] -> a -> SSM (a, [String])

instance (Arg a, Arg b) => Arg (a,b) where
    arg name names (x,y) = do
        (x', names')  <- arg name names  x
        (y', names'') <- arg name names' y
        return ((x',y'), names'')

class Res b where
    result :: String -> b -> SSM b

class Box b where
    box :: Arg a => String -> [String] -> (a -> b) -> (a -> b)

-- | Nullary functions can not be used with `box`, as `box` requires a function
-- to box. Here, we change that representation.
class BoxNullary b where
  boxNullary :: String -> b -> b

-- | There is a dummy `Arg` instance for `()`, which we can use to piggyback
-- on the box machinery that's already in place.
instance Res b => BoxNullary (SSM b) where
  boxNullary name b = box name [] (\() -> b) $ ()

instance Arg () where
  arg name names () = return ((), names)

instance (Arg b, Box c) => Box (b -> c) where
    box name xs f = curry (box name xs (uncurry f))

instance Res b => Box (SSM b) where
    box name xs f = \x -> do
        emit $ Procedure name
        (x',_) <- arg name xs x
        y'     <- f x'
        result name y'