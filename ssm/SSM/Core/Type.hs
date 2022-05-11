{- | This module talks about the types that are supported by SSM. -}
module SSM.Core.Type
    ( Type(..)
    , dereference
    , mkReference
    , isReference
    , SSMType(..)
    )where

import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Word                      ( Word32
                                                , Word64
                                                , Word8
                                                )

-- * Types

-- | Data types supported by the language
data Type
    = TUInt8    -- ^ Unsigned 8-bit integer
    | TUInt32   -- ^ Unsigned 32-bit integer
    | TUInt64   -- ^ Unsigned 64-bit integer
    | TInt32    -- ^ Signed 32-bit integer
    | TInt64    -- ^ Signed 64-bit integer
    | TBool     -- ^ Boolean type
    | TEvent    -- ^ Event type
    | Ref Type  -- ^ A reference to another type
    deriving (Eq, Show, Ord, Read)

-- | Dereference a type. Throws an error if the type is not a reference.
dereference :: Type -> Type
dereference (Ref t) = t
dereference t = error $ "not a reference type: can not dereference " ++ show t

-- | Turn a type into a reference to that type.
mkReference :: Type -> Type
mkReference = Ref

-- | Predicate to verify that a type is a reference to some other type.
isReference :: Type -> Bool
isReference (Ref _) = True
isReference _       = False

-- * The SSMType class

{-| The class of Haskell types that can be marshalled to a representation
in the SSM language. -}
class SSMType a where
    -- | Take a @proxy a@ and turn that into a `Type` that represents @a@.
    typeOf :: proxy a -> Type

instance SSMType Word8 where
    typeOf _ = TUInt8

instance SSMType Word32 where
    typeOf _ = TUInt32

instance SSMType Word64 where
    typeOf _ = TUInt64

instance SSMType Int32 where
    typeOf _ = TInt32

instance SSMType Int64 where
    typeOf _ = TInt64

instance SSMType Bool where
    typeOf _ = TBool

instance SSMType () where
    typeOf _ = TEvent
