{-# LANGUAGE DerivingVia #-}
module SSM.Core.Syntax
    ( -- * Types
      Type(..)
    , dereference
    , mkReference
    , isReference
    , SSMType(..)

      -- * References
    , Reference(..)
    , refType
    , refName

      -- * Expressions
    , SSMExp(..)
    , SSMLit(..)
    , UnaryOpE(..)
    , UnaryOpR(..)
    , BinOp(..)
    , expType

      -- * Names
    , Name(..)
    , getVarName

      -- * Statements & SSM monad
    , SSMStm(..)
    , SSM(..)
    , SSMSt(..)
    , runSSM
    , pureSSM
    , emit
    , fresh
    , getProcedureName
    ) where

import Control.Monad.State
    ( gets, modify, execState, MonadState, State, StateT(StateT) )

import Data.Int ( Int32, Int64 )
import Data.Word ( Word8, Word64 )

-- Types

-- | Data types supported by the language
data Type
    = TUInt8    -- ^ Unsigned 8-bit integer
    | TUInt64   -- ^ Unsigned 64-bit integer
    | TInt32    -- ^ Signed 32-bit integer
    | TInt64    -- ^ Signed 64-bit integer
    | TBool     -- ^ Boolean type
    | Ref Type  -- ^ A reference to another type
    deriving (Eq, Show, Read)

-- | Dereference a type. Throws an error if the type is not a reference.
dereference :: Type -> Type
dereference (Ref t) = t
dereference t       = error $ "not a reference type: can not dereference " ++ show t

-- | Turn a type into a reference to that type.
mkReference :: Type -> Type
mkReference = Ref

-- | Predicate to verify that a type is a reference to some other type.
isReference :: Type -> Bool
isReference (Ref _) = True
isReference _       = False

{-| The class of Haskell types that can be marshalled into a representation
in the SSM language.

-}
class SSMType a where
    typeOf :: proxy a -> Type

instance SSMType Word8 where
  typeOf _ = TUInt8

instance SSMType Word64 where
    typeOf _ = TUInt64

instance SSMType Int32 where
    typeOf _ = TInt32

instance SSMType Int64 where
    typeOf _ = TInt64

instance SSMType Bool where
    typeOf _ = TBool


-- References

-- | References in our language have a name and a type
type Reference
    = (String, Type)

-- | Type of a reference
refType :: Reference -> Type
refType (_,t) = t

-- | Name of a reference
refName :: Reference -> String
refName (n,_) = n


-- Expressions

-- | Expressions in the language take any of these forms
data SSMExp
    = Var Type String               -- ^ Variables
    | Lit Type SSMLit               -- ^ Literals
    | UOpE Type SSMExp UnaryOpE     -- ^ Unary operators on expressions
    | UOpR Type Reference UnaryOpR  -- ^ Unary operators on references
    | BOp Type SSMExp SSMExp BinOp  -- ^ Binary operators
    deriving (Eq, Show, Read)

-- | Literals take any of these forms
data SSMLit
    = LUInt8 Word8    -- ^ 8bit unsigned integers
    | LInt32 Int32    -- ^ Integer literals
    | LInt64 Int64    -- ^ 64bit integer literals
    | LUInt64 Word64  -- ^ 64bit unsigned integer literals
    | LBool Bool      -- ^ Boolean literals
    deriving (Eq, Show, Read)

-- | Expressions of unary operators on expressions
data UnaryOpE
    = Neg  -- ^ negation
    deriving (Show, Eq, Read)

-- | Expressions of unary operators on references
data UnaryOpR
    = Changed  -- ^ Expression represents if the reference has been written to
    deriving (Show, Eq, Read)

-- | Expressions of binary operators.
data BinOp
    = OPlus   -- ^ addition
    | OMinus  -- ^ subtraction
    | OTimes  -- ^ multiplication
    | OLT     -- ^ less-than
    | OEQ     -- ^ eq
    deriving (Eq, Show, Read)

-- | Return the type of an expression
expType :: SSMExp -> Type
expType (Var t _)     = t
expType (Lit t _)     = t
expType (UOpE t _ _)  = t
expType (UOpR t _ _)  = t
expType (BOp t _ _ _) = t


-- Names of variables

-- | Names in the language can either be freshly generated or captured by the source.
data Name
    = Fresh String  -- ^ Freshly generated name
    -- ^ Name that's been captured from the source. (Filename, line, column) name.
    | Captured (String,Int,Int) String
    deriving (Show, Eq, Read)

-- | Get the String version of a Name
getVarName :: Name -> String
getVarName (Fresh n)      = n
getVarName (Captured _ n) = n


-- SSM monad and statement type (mutually recursive)

{- | A high level, untyped version of the statements that make up an SSM program.
Ensuring type safety is the job of the Frontend. -}
data SSMStm
    -- | Variable/Stream operations
    = NewRef Name SSMExp       -- ^ Create a new named reference with an initial value
    | GetRef Name Reference    -- ^ Dereference a reference, place the result in a var
    | SetRef Reference SSMExp  -- ^ Set a reference
    {-| Set a local variable. Expression variables can currently only be created when
    they are given to a procedure as an argument, or by dereferencing a reference. -}
    | SetLocal SSMExp SSMExp

    -- | Control operations
    | If SSMExp (SSM ()) (Maybe (SSM ()))  -- ^ Conditional execution
    | While SSMExp (SSM ())                -- ^ Loop construct
            
    -- | SSM specific operations
    | After SSMExp Reference SSMExp  -- ^ Scheduled assignment
    | Wait [Reference]               -- ^ Wait for any of the references to be written to
    | Fork [SSM ()]                  -- ^ Fork a list of procedures

    -- | Procedure construction
    | Procedure String  -- ^ Marks the start of a procedure
    {-| Records the name an argument has and what value the procedure was applied to -}
    | Argument String String (Either SSMExp Reference)
    | Result String  -- ^ Mark the end of a procedure

{- | The state maintained by the SSM monad. A counter for generating fresh names and
a list of statements that make up the program. -}
data SSMSt = SSMSt { counter    :: Int
                   , statements :: [SSMStm]
                   }

-- | The SSM monad is used to build programs. It is a state monad the records statements.
newtype SSM a = SSM (State SSMSt a)
  deriving Functor            via State SSMSt
  deriving Applicative        via State SSMSt
  deriving Monad              via State SSMSt
  deriving (MonadState SSMSt) via State SSMSt

-- | Run a SSM program and get the statements from it.
runSSM :: SSM a -> [SSMStm]
runSSM (SSM program) = statements $ execState program (SSMSt 0 [])

-- | Take a list of statements and turn them into an SSM computation.
pureSSM :: [SSMStm] -> SSM ()
pureSSM stmts = modify $ \st -> st { statements = stmts }

-- | Emit an SSM statement.
emit :: SSMStm -> SSM ()
emit stm = modify $ \st -> st { statements = statements st ++ [stm]}

-- | Fetch a fresh name from the environment.
fresh :: SSM String
fresh = do
    i <- gets counter
    modify $ \st -> st { counter = i + 1 }
    return $ "v" ++ show i

{- | Get the name of a procedure, where the procedure is represented by a list of
statements that make up its body. -}
getProcedureName :: [SSMStm] -> String
getProcedureName (Procedure n:_) = n
getProcedureName _               = error "not a procedure"