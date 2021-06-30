{-| This module implements the core abstract syntax. This syntax is meant to be
completely detached from the embedded language, and could thus be a target for
a parser or something else (in our case the embedded language, however). The
rest of the compiler will work with this representation (compiler, interpreter,
pretty printer etc). -}
module SSM.Core.Syntax
    ( -- * SSM Core Syntax

      -- ** Types
      Type(..)
    , dereference
    , mkReference
    , isReference
    , SSMType(..)

      -- ** References
    , Reference(..)
    , refType
    , refName

      -- ** Expressions
    , SSMExp(..)
    , SSMLit(..)
    , UnaryOpE(..)
    , UnaryOpR(..)
    , BinOp(..)
    , expType

      -- ** Names
    , Name(..)
    , getVarName

      -- ** Statements
    , Stm(..)

      -- ** Procedures
    , Procedure(..)

      -- ** Programs
    , Program(..)
    , SSMProgram(..)
    ) where

--import qualified SSM.Core.Syntax as S

import Data.Int
import Data.Word
import qualified Data.Map as Map
import Control.Monad.State.Lazy
    ( forM, modify, runState, MonadState(put, get), State )

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

{-| The class of Haskell types that can be marshalled to a representation
in the SSM language. -}
class SSMType a where
    -- | Take a @proxy a@ and turn that into a `Type` that represents @a@.
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


-- Programs

{- | A lower level representation of the statements that make up the body of
an SSM program. -}
data Stm
    {-| Create a new reference with the given name, which references a value of the
    given type, with the initial value specified by the expression. -}
    = NewRef Name Type SSMExp
    {-| Dereference an expression and put the result in a variable with the given name &
    with the given type.-}
    | GetRef Name Type Reference
    | SetRef Reference SSMExp  -- ^ Set the value of a reference
    {-| Set the value of a local expression specified by the name, with the given type,
    with the new value specified by the expression. -}
    | SetLocal Name Type SSMExp

    | If SSMExp [Stm] [Stm]  -- ^ Conditional execution
    | While SSMExp [Stm]     -- ^ Loop construct
    | Skip                   -- ^ No-op

    -- | After d r v - After d units of time the reference r should get the new value v
    | After SSMExp Reference SSMExp
    | Wait [Reference]  -- ^ Wait for any of the references to be written to
    {-| Fork procedures. The procedures are now identified by their name, and the fork
    site contains only that name and the arguments to apply the function to. -}
    | Fork [(String, [Either SSMExp Reference])]
    deriving (Show, Eq, Read)

-- | A procedure has a name, parameter names & types and a body.
data Procedure = Procedure
    { -- | Name of the procedure.
      name      :: String
      -- | Parameter names and types of the procedure.
     , arguments :: [(String, Type)]
      -- | Statements that make up this procedure.
    , body      :: [Stm]
    } deriving (Eq, Show, Read)

{- | A program has an entry point, arguments to that entry point and a map that maps
procedure names to their definitions. -}
data Program = Program
    { -- | Name of the procedure that is the program entrypoint.
      entry :: String
      -- | Arguments the entrypoint was applied to.
    , args :: [Either SSMExp Reference]
      -- | Map that associates procedure names with their definitions.
    , funs :: Map.Map String Procedure
    } deriving (Show, Read)

-- | Class of types that can be converted to a `Program`.
class SSMProgram a where
  -- | This function takes an `a` and converts it to a `Program`
  toProgram :: a -> Program

instance SSMProgram Program where
  toProgram = id
