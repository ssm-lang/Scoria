{-| This module exports some global identifiers that the generated C code uses,
as well as some functions used to construct identifiers.
-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Identifiers
  ( -- * Type aliases
    CIdent(..)

      -- * Identifiers recognized by the C runtime system.
  , initialize_program
  , initialize_static_input_device
  , initialize_static_input_ble_scan_device
  , initialize_static_output_device
  , initialize_static_output_ble_scan_control_device
  , initialize_static_output_ble_broadcast_control_device
  , initialize_static_output_ble_broadcast_device
  , enable_ble_stack
  , top_return
  , top_parent
  , fork
  , act_enter
  , act_leave
  , event_on
  , sensitize
  , desensitize
  , unsched_event
  , entry_point
  , throw
  , exhausted_priority
  , now
  , never
  , pdep
  , pdeps
  , depthSub
  , depth_at_root
  , priority_at_root

      -- * Type names recognized by the the C runtime system.
  , time_t
  , trigger_t
  , priority_t
  , depth_t
  , stepf_t
  , act_t
  , sv_t
  , i32
  , u32
  , i64
  , u64
  , u8
  , event
  , bool

      -- * Constructing Identifiers from strings
      -- | These functions create identifiers from some known [pre|suf]fix.
  , act_
  , step_
  , enter_
  , trig_

      -- * Constructing SSM time macros from SSMTimeUnit
  --, units_

      -- * Constructing Identifiers from types
      {- | Some identifiers need to be prefixed or suffixed with some type information,
      such as @later_int@, @later_bool@ etc. We create identifiers like these and others
      by using these functions. -}
  , svt
  , initialize
  , assign
  , later

    -- * Debug-/trace-specific macros
  , debug_microtick
  , debug_trace
  ) where

import           SSM.Core

import           Language.C.Quote.GCC           ( cexp
                                                , cstm
                                                , cty
                                                )
import qualified Language.C.Syntax             as C

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Type alias for C identifiers.
type CIdent = String

-- | Name of top level program initialization function
initialize_program :: CIdent
initialize_program = "ssm_program_initialize"

-- | Name of the top level static input switch initialization function
initialize_static_input_device :: CIdent
initialize_static_input_device = "bind_static_input_device"

initialize_static_input_ble_scan_device :: CIdent
initialize_static_input_ble_scan_device = "bind_static_ble_scanning_device"

-- | Name of the top level static output initialization function
initialize_static_output_device :: CIdent
initialize_static_output_device = "bind_static_output_device"

initialize_static_output_ble_scan_control_device :: CIdent
initialize_static_output_ble_scan_control_device =
  "bind_static_output_ble_scan_control_device"

initialize_static_output_ble_broadcast_control_device :: CIdent
initialize_static_output_ble_broadcast_control_device =
  "bind_static_output_ble_broadcast_control_device"

initialize_static_output_ble_broadcast_device :: CIdent
initialize_static_output_ble_broadcast_device =
  "bind_static_ble_broadcast_device"

enable_ble_stack :: CIdent
enable_ble_stack = "enable_ble_stack"

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

-- | Name of top level parent activation record
top_parent :: CIdent
top_parent = "ssm_top_parent"

-- | Name of top level return step-function
entry_point :: CIdent
entry_point = "ssm_entry_point"

-- | Name of routine that forks procedures
fork :: CIdent
fork = "ssm_activate"

-- | Name of routine that initialized an activation record
act_enter :: CIdent
act_enter = "ssm_enter"

-- | Name of routine that deallocates an activation record
act_leave :: CIdent
act_leave = "ssm_leave"

-- | Name of routine that checks if a reference has been written to
event_on :: CIdent
event_on = "ssm_event_on"

-- | Name of routine that sensitizes a procedure
sensitize :: CIdent
sensitize = "ssm_sensitize"

-- | Name of routine that desensitizes a procedure
desensitize :: CIdent
desensitize = "ssm_desensitize"

-- | Name of routine that dequeues an event from the event queue
unsched_event :: CIdent
unsched_event = "ssm_unschedule"

-- | Name of routine that returns the current value of now.
now :: CIdent
now = "ssm_now"

-- | Name of routine that returns the current value of now.
never :: CIdent
never = "SSM_NEVER"

-- | Name of macro that throws an error.
throw :: CIdent
throw = "SSM_THROW"

-- | Exhausted priority
exhausted_priority :: C.Exp
exhausted_priority = [cexp|SSM_EXHAUSTED_PRIORITY|]

{- | Create C expressions that represent the new priorities and depths of the
initially scheduled processes. -}
-- pdeps :: Int -> C.Exp -> C.Exp -> [(C.Exp, C.Exp)]
-- pdeps cs currentPrio currentDepth =
--       [ let prio  = [cexp|$exp:currentPrio + ($int:(i-1) * (1 << $exp:depth))|]
--             depth = [cexp|$exp:currentDepth - $exp:(depthSub cs)|]
--         in (prio, depth)
--       | i <- [1..cs]
--       ]

pdeps :: Int -> C.Exp -> C.Exp -> [(C.Exp, C.Exp)]
pdeps cs currentPrio currentDepth =
  map (\k -> pdep k cs currentPrio currentDepth) [1..cs]

pdep :: Int -> Int -> C.Exp -> C.Exp -> (C.Exp, C.Exp)
pdep k cs currentPrio currentDepth =
  let prio = [cexp|$exp:currentPrio + ($int:(k-1) * (1 << $exp:depth))|]
      depth  = [cexp|$exp:currentDepth - $exp:(depthSub cs)|]
  in (prio, depth)

{- | Calculate the subexpression that should be subtracted from the current depth
in order to achieve the new depth of the processes to fork.

The argument is the number of new processes that are being forked. -}
depthSub :: Int -> C.Exp
depthSub k = [cexp|$int:(ceiling $ logBase (2 :: Double) $ fromIntegral $ k :: Int) |]

depth_at_root :: C.Exp
depth_at_root = [cexp|SSM_ROOT_DEPTH|]

priority_at_root :: C.Exp
priority_at_root = [cexp|SSM_ROOT_PRIORITY|]

-- | C type that represents model time
time_t :: C.Type
time_t = [cty|typename ssm_time_t|]

-- | C type that represents process priorities
priority_t :: C.Type
priority_t = [cty|typename ssm_priority_t|]

-- | C type that represents process depths
depth_t :: C.Type
depth_t = [cty|typename ssm_depth_t|]

-- | C type that represents triggers, aka processes that are sensitized on variables
trigger_t :: C.Type
trigger_t = [cty|struct ssm_trigger|]

-- | C type that represents the step function of a process
stepf_t :: C.Type
stepf_t = [cty|typename ssm_stepf_t|]

{---- Activation record identifiers ----}

-- | The type of the activation record base class.
act_t :: C.Type
act_t = [cty|struct ssm_act|]

-- | Obtain the name of the activation record struct for a routine.
act_ :: String -> CIdent
act_ routineName = "act_" ++ routineName ++ "_t"

-- | Obtain the name of the step function of a routine.
step_ :: String -> CIdent
step_ routineName = "step_" ++ routineName

-- | Obtain the name for the enter function of a routine.
enter_ :: String -> CIdent
enter_ routineName = "enter_" ++ routineName

-- | Obtain the name of each trigger for a routine.
trig_ :: Int -> CIdent
trig_ i = "trig" ++ show i

{---- Type identifiers ----}

-- | The type of the scheduled variable base class.
sv_t :: C.Type
sv_t = [cty|struct sv|]

i32 :: CIdent
i32 = "i32"

u32 :: CIdent
u32 = "u32"

i64 :: CIdent
i64 = "i64"

u64 :: CIdent
u64 = "u64"

u8 :: CIdent
u8 = "u8"

event :: CIdent
event = "event"

bool :: CIdent
bool = "bool"

-- | Obtain the name of the scheduled variable type.
svt :: CIdent -> CIdent
svt ty = "ssm_" ++ ty ++ "_t"

-- | Obtain the name of the initialize method.
initialize :: CIdent -> CIdent
initialize ty = "ssm_initialize_" ++ ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign :: CIdent -> CIdent
assign ty = "ssm_assign_" ++ ty

-- | Obtain the name of the later method for an SSM `Type`.
later :: CIdent -> CIdent
later ty = "ssm_later_" ++ ty

debug_microtick :: CIdent
debug_microtick = "SSM_DEBUG_MICROTICK"

debug_trace :: CIdent
debug_trace = "SSM_DEBUG_TRACE"
