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
  deriving (Show, Eq)

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
            | Changed Name Reference
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

data Type = TInt
          | TUInt8
          | TInt64
          | TUInt64
          | TBool
          | Ref Type
  deriving (Eq, Show, Generic, NFData)

class SSMType a where
    typeOf :: proxy a -> Type

instance SSMType Int where
    typeOf _ = TInt

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
expType (UOp t _ _)   = t
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

instance Num SSMExp where
  e1 + e2       = BOp (expType e1) e1 e2 OPlus
  e1 * e2       = BOp (expType e1) e1 e2 OTimes
  e1 - e2       = BOp (expType e1) e1 e2 OMinus
  fromInteger i = if T.typeOf i == T.typeOf (1 :: Int)
                    then Lit TInt   $ LInt   $ fromInteger i
                    else Lit TInt64 $ LInt64 $ fromInteger i
  negate e      = UOp (expType e) e Neg

-- | SSM expressions
data SSMExp = Var Type String               -- ^ Variables
            | Lit Type SSMLit               -- ^ Literals
            | UOp Type SSMExp UnaryOp       -- ^ Unary operators
            | BOp Type SSMExp SSMExp BinOp  -- ^ Binary operators
  deriving (Eq, Generic, NFData, Show)

-- | SSM literals
data SSMLit = LInt Int        -- ^ Integer literals
            | LUInt8 Word8    -- ^ 8bit unsigned integers
            | LInt64 Int64    -- ^ 64bit integer literals
            | LUInt64 Word64  -- ^ 64bit unsigned integer literals
            | LBool Bool      -- ^ Boolean literals
  deriving (Eq, Generic, NFData, Show)

{-- | SSM unary operators -}
data UnaryOp = Neg  -- ^ negation
  deriving (Show, Eq, Generic, NFData)

{-- | SSM binary operators. We use phantom types to represent the two argument types and
the result type of the operator. E.g OLT 2 3 :: BinOp Int Int Bool -}
data BinOp = OPlus   -- ^ addition
           | OMinus  -- ^ subtraction
           | OTimes  -- ^ multiplication
           | OLT     -- ^ less-than
           | OEQ     -- ^ eq
  deriving (Eq, Generic, NFData, Show)

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

instance (Arg b, Box c) => Box (b -> c) where
    box name xs f = curry (box name xs (uncurry f))

instance Res b => Box (SSM b) where
    box name xs f = \x -> do
        emit $ Procedure name
        (x',_) <- arg name xs x
        y'     <- f x'
        result name y'
