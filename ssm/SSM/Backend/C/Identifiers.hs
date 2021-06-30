{-| This module exports some identifiers that are commonly reoccuring in the
generated C code. They are declared here to avoid repetition in the files
where they are referenced.-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Identifiers
    ( -- * Identifiers
      CIdent
    , top_return
    , fork
    , act_enter
    , event_on
    , sensitize
    , desensitize
    , dequeue_event

      -- * C Types
    , time_t
    , trigger_t
    , priority_t
    , depth_t
    , stepf_t
    , uint16_t
    , bool_t
    , act_t
    , sv_t

      -- * Constructing Identifiers from strings
    , act_
    , step_
    , enter_
    , trig_

      -- * Constructing Identifiers from types
    , typeId
    , svt_
    , initialize_
    , assign_
    , later_
    , update_

    ) where

import SSM.Core.Syntax

import Language.C.Quote.GCC ( cty )
import qualified Language.C.Syntax             as C

{-------- C identifiers --------}
-- These variables represent the magic identifiers that must be coordinated
-- between the C runtime and the generated code, as well as some helpers to
-- generate C type nodes for user-defined types.

-- | Type alias for C identifiers.
type CIdent = String

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

-- | Name of routine that forks procedures
fork :: CIdent
fork = "fork_routine"

-- | Name of routine that initialized an activation record
act_enter :: CIdent
act_enter = "enter"

-- | Name of routine that checks if a reference has been written to
event_on :: CIdent
event_on = "event_on"

-- | Name of routine that sensitizes a procedure
sensitize :: CIdent
sensitize = "sensitize"

-- | Name of routine that desensitizes a procedure
desensitize :: CIdent
desensitize = "desensitize"

-- | Name of routine that dequeues an event from the event queue
dequeue_event :: CIdent
dequeue_event = "dequeue_event"

-- | C type that represents model time
time_t :: C.Type
time_t = [cty|typename peng_time_t|]

-- | C type that represents triggers, aka processes that are sensitized on variables
trigger_t :: C.Type
trigger_t = [cty|typename trigger_t|]

-- | C type that represents process priorities
priority_t :: C.Type
priority_t = [cty|typename priority_t|]

-- | C type that represents process depths
depth_t :: C.Type
depth_t = [cty|typename depth_t|]

-- | C type that represents the step function of a process
stepf_t :: C.Type
stepf_t = [cty|typename stepf_t|]

-- | C type that represents 16 bit unsigned integers
uint16_t :: C.Type
uint16_t = [cty|typename uint16_t|]

-- | C type that represents booleans
bool_t :: C.Type
bool_t = [cty|typename bool|]

{---- Activation record identifiers ----}

-- | The type of the activation record base class.
act_t :: C.Type
act_t = [cty|typename act_t|]

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
sv_t = [cty|typename sv_t|]

-- | Maps SSM `Type` to identifier of base type.
--
-- Note that this unwraps reference types and returns the base type.
typeId :: Type -> CIdent
typeId TInt32  = "int32"
typeId TInt64  = "int64"
typeId TUInt64 = "uint64"
typeId TUInt8  = "uint8"
typeId TBool   = "bool"
typeId (Ref t) = typeId t

-- | Obtain the name of the scheduled variable type for an SSM `Type`.
svt_ :: Type -> C.Type
svt_ ty = [cty|typename $id:("sv_" ++ typeId ty ++ "_t")|]

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: Type -> CIdent
initialize_ ty = "initialize_" ++ typeId ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: Type -> CIdent
assign_ ty = "assign_" ++ typeId ty

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: Type -> CIdent
later_ ty = "later_" ++ typeId ty

-- | Obtain the name of the update callback for an SSM `Type`.
update_ :: Type -> CIdent
update_ ty = "update_" ++ typeId ty
