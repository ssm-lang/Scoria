{-| This module exports some global identifiers that the generated C code uses,
as well as some functions used to construct identifiers.
-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Identifiers
  ( -- * Type aliases
    CIdent(..)

      -- * Identifiers recognized by the C runtime system.
  , initialize_program
  , top_return
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

      -- * Type names recognized by the the C runtime system.
  , time_t
  , trigger_t
  , priority_t
  , depth_t
  , stepf_t
  , act_t
  , sv_t
      {- | These are (a subset of the) types that the runtime system includes and uses
      internally. These are just the ones that the code generator needs to talk about. -}
  , uint16_t
  , bool_t

      -- * Constructing Identifiers from strings
      -- | These functions create identifiers from some known [pre|suf]fix.
  , act_
  , step_
  , enter_
  , trig_

      -- * Constructing SSM time macros from SSMTimeUnit
  , units_

      -- * Constructing Identifiers from types
      {- | Some identifiers need to be prefixed or suffixed with some type information,
      such as @later_int@, @later_bool@ etc. We create identifiers like these and others
      by using these functions. -}
  , svt_
  , initialize_
  , assign_
  , later_
  , basetype

    -- * Debug-/trace-specific macros
  , debug_microtick
  , debug_trace

    -- * Accessing references
    {- | These functions help the code generator compile a reference into a
    C-expression that references the same reference. -}
  , refPtr
  , refVal
  , refSV
  ) where

import           SSM.Core.Syntax

import           Language.C.Quote.GCC           ( cexp
                                                , cstm
                                                , cty
                                                )
import qualified Language.C.Syntax             as C
import           SSM.Backend.C.Types

-- | Use snake_case for c literals
{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Type alias for C identifiers.
type CIdent = String

-- | Name of top level program initialization function
initialize_program :: CIdent
initialize_program = "ssm_initialize_program"

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

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

exhausted_priority :: C.Exp
exhausted_priority = [cexp|SSM_EXHAUSTED_PRIORITY|]

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

-- | C type that represents 16 bit unsigned integers
uint16_t :: C.Type
uint16_t = [cty|typename uint16_t|]

-- | C type that represents booleans
bool_t :: C.Type
bool_t = [cty|typename bool|]

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

-- | Obtain the name of the scheduled variable type for an SSM `Type`.
svt_ :: Type -> C.Type
svt_ ty = [cty|typename $id:("ssm_" ++ baseTypeId ty ++ "_t")|]

basetype :: Type -> C.Type
basetype t = [cty|typename $id:(baseTypeId t)|]

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: Type -> CIdent
initialize_ ty = "ssm_initialize_" ++ baseTypeId ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: Type -> CIdent
assign_ ty = "ssm_assign_" ++ baseTypeId ty

-- | Obtain the name of the unit macro for an `SSMTimeUnit`.
units_ :: SSMTimeUnit -> CIdent
units_ SSMNanosecond  = "SSM_NANOSECOND"
units_ SSMMicrosecond = "SSM_MICROSECOND"
units_ SSMMillisecond = "SSM_MILLISECOND"
units_ SSMSecond      = "SSM_SECOND"
units_ SSMMinute      = "SSM_MINUTE"
units_ SSMHour        = "SSM_HOUR"

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: Type -> CIdent
later_ ty = "ssm_later_" ++ baseTypeId ty

debug_microtick :: CIdent
debug_microtick = "SSM_DEBUG_MICROTICK"

debug_trace :: CIdent
debug_trace = "SSM_DEBUG_TRACE"

{- | Given a reference and a list of local references, this function will return
a C expression that holds a pointer to the reference. -}
refPtr :: Reference -> [Reference] -> C.Exp
refPtr r@(Dynamic _) lrefs =
  if r `elem` lrefs
    then [cexp| &acts->$id:(refName r) |]
    else [cexp| acts->$id:(refName r)  |]
-- Static references can be referenced without an activation record
refPtr r@(Static _) _ = [cexp| &$id:(refName r) |]

{- | Given a reference and a list of local references, this function will return a
C expression that holds a pointer to the internal sv-component of the processes
activation record. -}
refSV :: Reference -> [Reference] -> C.Exp
refSV r@(Dynamic _) lrefs =
  if r `elem` lrefs
    then [cexp| &acts->$id:(refName r).sv |]
    else [cexp| &acts->$id:(refName r)->sv|]
refSV r@(Static _) _ = [cexp| &$id:(refName r).sv|]

{- | Given a Reference, this function will return a C expression that holds
the value of that reference. -}
refVal :: Reference -> [Reference] -> C.Exp
refVal r@(Dynamic _) lrefs =
  if r `elem` lrefs
    then [cexp| acts->$id:(refName r).value|]
    else [cexp| acts->$id:(refName r)->value|]
refVal r@(Static _) _      = [cexp| $id:(refName r).value |]
