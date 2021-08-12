{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Types where

import           SSM.Core.Syntax (Ident(..), Type(..))
import SSM.Backend.C.Identifiers

import           Language.C.Quote.GCC           ( cexp, cty, cstm)
import qualified Language.C.Syntax             as C
import qualified SSM.Interpret.Trace           as T


-- | Unwrap one layer of reference from an SSM 'Type'.
derefType :: Type -> Maybe Type
derefType (Ref t) = Just t
derefType _ = Nothing

-- | Construct a 'C.Type' from an identifier.
t_ :: CIdent -> C.Type
t_ t = [cty|typename $id:t|]

-- | Obtain typename for the C type we use to represent data of SSM 'Type'.
base :: Type -> CIdent
base TInt32 = u32
base TInt64 = u32
base TUInt64 = u64
base TUInt8 = u64
base TBool = bool
base TEvent = event
base (Ref _) = fail "No base data type for references"

base_ :: Type -> C.Type
base_ = t_ . base

svt_ :: Type -> C.Type
svt_ = t_ . svt . base

initialize_ :: Type -> C.Exp -> C.Exp
initialize_ t e = [cexp|$id:(initialize $ base t)($exp:e)|]

assign_ :: Type -> C.Exp -> C.Exp -> C.Exp -> C.Exp
assign_ (Ref ty@TEvent) lhs prio _ = [cexp|$id:(assign $ base ty)($exp:lhs, $exp:prio)|]
assign_ ty lhs prio rhs = [cexp|$id:(assign $ base ty)($exp:lhs, $exp:prio, $exp:rhs)|]

later_ :: Type -> C.Exp -> C.Exp -> C.Exp -> C.Exp
later_ (Ref ty@TEvent) lhs time _ = [cexp|$id:(later $ base ty)($exp:lhs, $exp:time)|]
later_ ty lhs time rhs = [cexp|$id:(later $ base ty)($exp:lhs, $exp:time, $exp:rhs)|]

trace_ :: Type -> Ident -> C.Exp -> C.Exp
trace_ t n v = [cexp|$id:debug_trace(TODO)|]

    -- actLocalVarS nt = show $ T.ActVar $ varFmt nt 
    -- debugLocal :: Reference -> C.Stm
    -- debugLocal r
    --   | baseType (refType r) == TEvent
    --   = [cstm|if ($exp:initialized) $id:debug_trace($exp:fmt);|]
    --   | otherwise
    --   = [cstm|if ($exp:initialized) $id:debug_trace($exp:fmt, $exp:val);|]
    --  where
    --   initialized :: C.Exp
    --   initialized =
    --     [cexp|$id:acts->$id:(refName r).sv.last_updated != $id:never|]


    --   fmt = [cexp|$string:(actLocalVarS (refIdent r, refType r))|]

    --   val :: C.Exp
    --   val = [cexp|$id:acts->$id:(refName r).value|]

    -- debugArg :: (Ident, Type) -> C.Stm
    -- debugArg (n, t) | baseType t == TEvent = [cstm|$id:debug_trace($exp:fmt);|]
    --                 | otherwise = [cstm|$id:debug_trace($exp:fmt, $exp:val);|]
    --  where
    --   fmt :: C.Exp
    --   fmt = [cexp|$string:(actLocalVarS (n, t))|]

    --   val :: C.Exp
    --   val | isReference t = [cexp|$id:acts->$id:(identName n)->value|]
    --       | otherwise     = [cexp|$id:acts->$id:(identName n)|]



-- -- | Maps SSM `Type` to identifier of the base type.
-- baseTypeId :: Type -> String
-- baseTypeId = typeId . baseType
--  where
--   typeId TInt32  = "i32"
--   typeId TInt64  = "i64"
--   typeId TUInt64 = "u64"
--   typeId TUInt8  = "u8"
--   typeId TBool   = "bool"
--   typeId TEvent  = "event"
--   typeId t = fail "baseTypeId: no basetype for type " ++ show t

-- -- | Obtain formatter to show given type's base type as an integer.
-- intFmt :: Type -> String
-- intFmt = fmt . baseType
--  where
--   fmt TInt64  = "%ld"
--   fmt TInt32  = "%d"
--   fmt TBool   = "%u"
--   fmt TUInt64 = "%lu"
--   fmt TUInt8  = "%u"
--   fmt t = fail $ "intFmt: no formatter type for type " ++ show t

-- varFmt :: (Ident, Type) -> T.VarVal
-- varFmt (n, t) | baseType t == TEvent = T.VarVal (identName n) (baseType t) T.UnitType
--               | otherwise = T.VarVal (identName n) (baseType t) $ T.IntegralFmt $ intFmt t
