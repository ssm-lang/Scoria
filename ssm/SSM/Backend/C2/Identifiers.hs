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
ssm_act_t = [cty| struct ssm_act_t |]

ssm_priority_t :: C.Type
ssm_priority_t = [cty| typename ssm_priority_t |]

ssm_depth_t :: C.Type
ssm_depth_t = [cty| typename ssm_depth_t |]

ssm_value_t :: C.Type
ssm_value_t = [cty| typename ssm_value_t |]

cont :: String
cont = "cont"

asType :: String -> C.Type
asType typ = [cty| typename $id:typ |]

marshal :: String
marshal = "ssm_marshal"

unmarshal :: String
unmarshal = "ssm_unmarshal"

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

ssm_fork :: String
ssm_fork = "ssm_fork"

accessRef :: Reference -> String
accessRef r
  | isStatic r = cont <> "->" <> refName r
  | otherwise  = refName r
