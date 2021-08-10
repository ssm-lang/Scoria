module SSM.Backend.C.Types where

import           SSM.Core.Syntax (Ident(..), Type(..))
import SSM.Backend.C.Identifiers

import           Language.C.Quote.GCC           ( cexp )
import qualified Language.C.Syntax             as C
import qualified SSM.Interpret.Trace           as T


-- | Unwrap one layer of reference from an SSM 'Type'.
derefType :: Type -> Maybe Type
derefType (Ref t) = Just t
derefType _ = Nothing


-- | Obtain typename for the C type we use to represent an SSM 'Type'.
baseDataType :: Type -> CIdent
baseDataType TInt32 = u32
baseDataType TInt64 = u32
baseDataType TUInt64 = u64
baseDataType TBool = bool
baseDataType TEvent = event
baseDataType _ = undefined



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
  typeId t = fail "baseTypeId: no basetype for type " ++ show t

-- | Obtain formatter to show given type's base type as an integer.
intFmt :: Type -> String
intFmt = fmt . baseType
 where
  fmt TInt64  = "%ld"
  fmt TInt32  = "%d"
  fmt TBool   = "%u"
  fmt TUInt64 = "%lu"
  fmt TUInt8  = "%u"
  fmt t = fail $ "intFmt: no formatter type for type " ++ show t

varFmt :: (Ident, Type) -> T.VarVal
varFmt (n, t) | baseType t == TEvent = T.VarVal (identName n) (baseType t) T.UnitType
              | otherwise = T.VarVal (identName n) (baseType t) $ T.IntegralFmt $ intFmt t
