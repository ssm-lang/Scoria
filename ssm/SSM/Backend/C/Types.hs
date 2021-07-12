module SSM.Backend.C.Types where

import           SSM.Core.Syntax

import           Language.C.Quote.GCC           ( cexp )
import qualified Language.C.Syntax             as C
import qualified SSM.Interpret.Trace           as T

-- | Obtain base type for a type, i.e., unwrapping all references.
baseType :: Type -> Type
baseType (Ref t) = baseType t
baseType t       = t

-- | Maps SSM `Type` to identifier of the base type.
baseTypeId :: Type -> String
baseTypeId = typeId . baseType
 where
  typeId TInt32  = "i32"
  typeId TInt64  = "i64"
  typeId TUInt64 = "u64"
  typeId TUInt8  = "u8"
  typeId TBool   = "bool"
  typeId TEvent  = "event"

-- | Obtain formatter to show given type's base type as an integer.
intFmt :: Type -> String
intFmt = fmt . baseType
 where
  fmt TInt64  = "%ld"
  fmt TInt32  = "%d"
  fmt TBool   = "%u"
  fmt TUInt64 = "%lu"
  fmt TUInt8  = "%u"

varFmt :: (Ident, Type) -> T.VarVal
varFmt (n, t) | baseType t == TEvent = T.VarVal (identName n) (baseType t) T.UnitType
              | otherwise = T.VarVal (identName n) (baseType t) $ T.IntegralFmt $ intFmt t
