{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C2.Identifiers where

import SSM.Core

import           Language.C.Quote.GCC
import qualified Language.C.Syntax as C

struct_ssm_act :: C.Type
struct_ssm_act = [cty| struct ssm_act |]

struct_ssm_trigger :: C.Type
struct_ssm_trigger = [cty| struct ssm_trigger|]

act_ :: String -> String
act_ routineName = "act_" <> routineName <> "_t"

step_ :: String -> String
step_ routineName = "step_" <> routineName

ssm_enter_ :: String -> String
ssm_enter_ routineName = "ssm_enter_" <> routineName

ssm_act_t :: C.Type
ssm_act_t = [cty| typename ssm_act_t |]

ssm_to_sv :: String
ssm_to_sv = "ssm_to_sv"

ssm_priority_t :: C.Type
ssm_priority_t = [cty| typename ssm_priority_t |]

ssm_depth_t :: C.Type
ssm_depth_t = [cty| typename ssm_depth_t |]

ssm_value_t :: C.Type
ssm_value_t = [cty| typename ssm_value_t |]

ssm_time_t :: C.Type
ssm_time_t = [cty| typename ssm_time_t |]

ssm_new_sv :: String
ssm_new_sv = "ssm_new_sv"

cont :: String
cont = "cont"

act :: String
act = "act"

priority :: String
priority = "priority"

asType :: String -> C.Type
asType typ = [cty| typename $id:typ |]

marshal :: String
marshal = "ssm_marshal"

unmarshal :: String
unmarshal = "ssm_unmarshal"

nil_pointer :: String
nil_pointer = "nil_pointer"

is_nil :: String
is_nil = "is_nil"

ssm_new_time :: String
ssm_new_time = "ssm_new_time"

ssm_time_read :: String
ssm_time_read = "ssm_time_read"

ssm_new :: String
ssm_new = "ssm_new"

ssm_sv_init :: String
ssm_sv_init = "ssm_sv_init"

ssm_builtin :: String
ssm_builtin = "SSM_BUILTIN"

ssm_sv_k :: String
ssm_sv_k = "SSM_SV_K"

ssm_deref :: String
ssm_deref = "ssm_deref"

ssm_enter :: String
ssm_enter = "ssm_enter"

ssm_dup :: String
ssm_dup = "ssm_dup"

ssm_drop :: String
ssm_drop = "ssm_drop"

ssm_leave :: String
ssm_leave = "ssm_leave"

ssm_later :: String
ssm_later = "ssm_later"

ssm_now :: String
ssm_now = "ssm_now"

ssm_assign :: String
ssm_assign = "ssm_assign"

ssm_sensitize :: String
ssm_sensitize = "ssm_sensitize"

ssm_desensitize :: String
ssm_desensitize = "ssm_desensitize"

ssm_activate :: String
ssm_activate = "ssm_activate"

ssm_program_init :: String
ssm_program_init = "ssm_program_init"

ssm_program_exit :: String
ssm_program_exit = "ssm_program_exit"

ssm_top_parent :: String
ssm_top_parent = "ssm_top_parent"

ssm_root_priority :: String
ssm_root_priority = "SSM_ROOT_PRIORITY"

ssm_root_depth :: String
ssm_root_depth = "SSM_ROOT_DEPTH"

ssm_never :: String
ssm_never = "SSM_NEVER"

ssm_throw :: String
ssm_throw = "SSM_THROW"

ssm_exhausted_priority :: String
ssm_exhausted_priority = "SSM_EXHAUSTED_PRIORITY"

accessRef :: Reference -> String
accessRef r
  | isDynamic r = cont <> "->" <> refName r
  | otherwise  = refName r

baseType :: Type -> C.Type
baseType (Ref _) = error "baseType of ref, what is the intended result?"
baseType TUInt8  = [cty| typename uint8_t  |]
baseType TUInt32 = [cty| typename uint32_t |]
baseType TUInt64 = [cty| typename uint64_t |]
baseType TInt32  = [cty| typename int32_t  |]
baseType TInt64  = [cty| typename int64_t  |]
baseType TBool   = [cty| typename bool     |]
baseType TEvent  = [cty| typename uint8_t  |]


-- debug stuff

debug_microtick :: String
debug_microtick = "SSM_DEBUG_MICROTICK"

debug_trace :: String
debug_trace = "SSM_DEBUG_TRACE"
