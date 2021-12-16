{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Types where

import           SSM.Core.Syntax
import           SSM.Core.Type
import           SSM.Core.Ident

import           SSM.Backend.C.Identifiers

import qualified SSM.Trace.Trace           as T

import           Language.C.Quote.GCC           ( cexp
                                                , cstm
                                                , cty
                                                )
import qualified Language.C.Syntax             as C


-- | Construct a 'C.Type' from an identifier.
t_ :: CIdent -> C.Type
t_ t = [cty|typename $id:t|]

-- | Helper function to "dereference" an SSM 'Type'.
stripRef :: Type -> Type
stripRef (Ref t) = t
stripRef t       = error $ "Not a reference: " ++ show t

-- | Obtain typename for the C type we use to represent data of SSM 'Type'.
--
-- Only supports non-Ref types; use 'stripRef' to strip away 'Ref' when needed.
base :: Type -> CIdent
base TInt32  = u32
base TInt64  = u64
base TUInt64 = u64
base TUInt32 = u32  
base TUInt8  = u8
base TBool   = bool
base TEvent  = event
base (Ref t) = error $ "No base data type for reference: " ++ show t

-- | Obtain C type we use to represent data of SSM 'Type'.
base_ :: Type -> C.Type
base_ = t_ . base

-- | Obtain scheduled variable type used to represent data of SSM 'Type'.
svt_ :: Type -> C.Type
svt_ = t_ . svt . base

-- | Synthesize call to initialize method of an SV.
initialize_ :: Type -> C.Exp -> C.Exp
initialize_ t e = [cexp|$id:(initialize $ base t)($exp:e)|]

-- | Synthesize call to type-specific assign method of an SV.
assign_ :: Type -> C.Exp -> C.Exp -> C.Exp -> C.Exp
assign_ (Ref t) _ _ _ =
  error $  "assign_ expects non-reference type, instead got: " ++ show t
assign_ TEvent lhs prio _ =
  [cexp|$id:(assign $ base TEvent)($exp:lhs, $exp:prio)|]
assign_ ty lhs prio rhs =
  [cexp|$id:(assign $ base ty)($exp:lhs, $exp:prio, $exp:rhs)|]

-- | Synthesize call to type-specific later method of an SV.
later_ :: Type -> C.Exp -> C.Exp -> C.Exp -> C.Exp
later_ (Ref t) _ _ _ =
  error $  "later_ expects non-reference type, instead got: " ++ show t
later_ TEvent lhs time _ =
  [cexp|$id:(later $ base TEvent)($exp:lhs, $exp:time)|]
later_ ty lhs time rhs =
  [cexp|$id:(later $ base ty)($exp:lhs, $exp:time, $exp:rhs)|]

-- | Synthesize trace statement for an SV.
trace_ :: Type -> Ident -> C.Exp -> C.Exp
trace_ (Ref t) n _ =
  error $ "Cannot trace variable " ++ show n ++ " of reference type: " ++ show t
trace_ TEvent n v = [cexp|$id:debug_trace($string:fmt)|]
 where
  fmt :: String
  fmt = show $ T.ActVar $ T.VarVal (identName n) TEvent T.UnitType
trace_ t n v = [cexp|$id:debug_trace($string:fmt, $exp:(signed_ t v))|]
 where
  fmt :: String
  fmt = show $ T.ActVar $ T.VarVal (identName n) t $ T.IntegralFmt ifmt

  ifmt :: String
  ifmt = case t of
    TInt64  -> "%ld"
    TInt32  -> "%d"
    TBool   -> "%u"
    TUInt64 -> "%lu"
    TUInt32 -> "%lu"
    TUInt8  -> "%u"
    t       -> error $ "intFmt: no formatter type for type " ++ show t

-- | When the given type is signed, this function will produce a cast to the
-- signed presentation of that type; otherwise, it is left the same.
--
-- TODO: check with the C standard that this cast is always valid.
signed_ :: Type -> C.Exp -> C.Exp
signed_ TInt32 e = [cexp|($ty:(t_ i32)) $exp:e|]
signed_ TInt64 e = [cexp|($ty:(t_ i64)) $exp:e|]
signed_ t      e = [cexp|($ty:(t_ (base t))) $exp:e|]
--signed_ _      e = e
