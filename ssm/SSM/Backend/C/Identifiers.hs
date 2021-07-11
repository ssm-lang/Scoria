{-| This module exports some identifiers that are commonly reoccuring in the
generated C code. They are declared here to avoid repetition in the files
where they are referenced.-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Identifiers
    ( -- * Identifiers
      {- | These are identifiers that are recognized by the C runtime system, and can be
      used while generating code which targets the runtime system. -}
      CIdent
    , top_return
    , fork
    , act_enter
    , act_leave
    , event_on
    , sensitize
    , desensitize
    , unsched_event

      -- * C Types
      -- | These are types that the runtime system declares.
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

      -- * Constructing Identifiers from types
      {- | Some identifiers need to be prefixed or suffixed with some type information,
      such as @later_int@, @later_bool@ etc. We create identifiers like these and others
      by using these functions. -}
    , typeId
    , svt_
    , initialize_
    , assign_
    , later_

    ) where

import SSM.Core.Syntax

import Language.C.Quote.GCC ( cty )
import qualified Language.C.Syntax             as C

-- | Type alias for C identifiers.
type CIdent = String

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

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

-- | C type that represents model time
time_t :: C.Type
time_t = [cty|typename ssm_time_t |]

-- | C type that represents process priorities
priority_t :: C.Type
priority_t = [cty|typename ssm_priority_t|]

-- | C type that represents process depths
depth_t :: C.Type
depth_t = [cty|typename ssm_depth_t|]

-- | C type that represents triggers, aka processes that are sensitized on variables
trigger_t :: C.Type
trigger_t = [cty| struct ssm_trigger |]

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

-- | Maps SSM `Type` to identifier of base type.
--
-- Note that this unwraps reference types and returns the base type.
typeId :: Type -> CIdent
typeId TInt32  = "i32"
typeId TInt64  = "i64"
typeId TUInt64 = "u64"
typeId TUInt8  = "u8"
typeId TBool   = "bool"
typeId (Ref t) = typeId t

-- | Obtain the name of the scheduled variable type for an SSM `Type`.
svt_ :: Type -> C.Type
svt_ ty = [cty|typename $id:("ssm_" ++ typeId ty ++ "_t")|]

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: Type -> CIdent
initialize_ ty = "ssm_initialize_" ++ typeId ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: Type -> CIdent
assign_ ty = "ssm_assign_" ++ typeId ty

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: Type -> CIdent
later_ ty = "ssm_later_" ++ typeId ty
