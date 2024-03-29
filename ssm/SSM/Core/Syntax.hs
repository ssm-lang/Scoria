module SSM.Core.Syntax
  ( SSMExp(..)
  , SSMLit(..)
  , UnaryOpE(..)
  , UnaryOpR(..)
  , BinOp(..)
  , expType
  , isVar
  , Stm(..)
  ) where

import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Word                      ( Word32
                                                , Word64
                                                , Word8
                                                )

import           SSM.Core.Ident                 ( Ident )
import           SSM.Core.Reference             ( Reference )
import           SSM.Core.Type                  ( Type(..), SSMType(..) )

-- Expressions

-- | Expressions in the language take any of these forms
data SSMExp
    = Var Type Ident                -- ^ Variables
    | Lit Type SSMLit               -- ^ Literals
    | UOpE Type SSMExp UnaryOpE     -- ^ Unary operators on expressions
    | UOpR Type Reference UnaryOpR  -- ^ Unary operators on references
    | BOp Type SSMExp SSMExp BinOp  -- ^ Binary operators
    deriving (Eq, Show, Read)

-- | Literals take any of these forms
data SSMLit
    = LUInt8 Word8    -- ^ 8bit unsigned integers
    | LUInt32 Word32  -- ^ 32bit unsigned integers
    | LUInt64 Word64  -- ^ 64bit unsigned integer literals
    | LInt32 Int32    -- ^ Integer literals
    | LInt64 Int64    -- ^ 64bit integer literals
    | LBool Bool      -- ^ Boolean literals
    | LEvent          -- ^ Event literal
    deriving (Eq, Show, Read)

-- | Expressions of unary operators on expressions
data UnaryOpE
    = Neg  -- ^ Numerical negation
    | Not  -- ^ Boolean negation
    deriving (Show, Eq, Read)

-- | Expressions of unary operators on references
data UnaryOpR
    = Changed  -- ^ Expression represents if the reference has been written to
    | Deref    -- ^ Dereference/sample the value of a reference
    deriving (Show, Eq, Read)

-- | Expressions of binary operators.
data BinOp
    = OPlus   -- ^ addition
    | OMinus  -- ^ subtraction
    | OTimes  -- ^ multiplication
    | ODiv    -- ^ division
    | ORem    -- ^ remainder
    | OMin    -- ^ minimum of two numerical operands
    | OMax    -- ^ maximum of two numerical operands
    | OLT     -- ^ less-than
    | OEQ     -- ^ eq
    | OAnd    -- ^ boolean conjunction
    | OOr     -- ^ boolean disjunction
    | OLShift -- ^ left shift
    | ORShift -- ^ right shift
    | OBAnd   -- ^ bit conjunction
    | OBOr    -- ^ bit disjunction
    | OBXor   -- ^ bit xor
    deriving (Eq, Show, Read)

-- | Return the type of an expression
expType :: SSMExp -> Type
expType (Var t _    ) = t
expType (Lit t _    ) = t
expType (UOpE t _ _ ) = t
expType (UOpR t _ _ ) = t
expType (BOp t _ _ _) = t

-- | Is the expression a variable?
isVar :: SSMExp -> Bool
isVar (Var _ _) = True
isVar _         = False

{- | A lower level representation of the statements that make up the body of
an SSM program. -}
data Stm
    {-| Create a new reference with the given name, which references a value of the
    given type, with the initial value specified by the expression. -}
    = NewRef Ident Type SSMExp
    | SetRef Reference SSMExp  -- ^ Set the value of a reference
    {-| Set the value of a local expression specified by the name, with the given type,
    with the new value specified by the expression. -}
    | SetLocal Ident Type SSMExp

    | If SSMExp [Stm] [Stm]  -- ^ Conditional execution
    | While SSMExp [Stm]     -- ^ Loop construct
    | Skip                   -- ^ No-op

    {- | @After d r v@ - After @d@ units of time the reference @r@ should get the new
    value @v@. -}
    | After SSMExp Reference SSMExp
    | Wait [Reference]  -- ^ Wait for any of the references to be written to
    {-| Fork procedures. The procedures are now identified by their name, and the fork
    site contains only that name and the arguments to apply the function to. -}
    | Fork [(Ident, [Either SSMExp Reference])]
    deriving (Show, Eq, Read)
