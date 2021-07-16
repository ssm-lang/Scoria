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

      -- * Accessing references
      {- | These functions help the code generator compile a reference into a
      C-expression that references the same reference. -}
    , refPtr
    , refVal
    ) where

import SSM.Core.Syntax

import Language.C.Quote.GCC ( cty, cexp )
import qualified Language.C.Syntax             as C

-- | Type alias for C identifiers.
type CIdent = String

-- | Name of top level return step-function
top_return :: CIdent
top_return = "top_return"

-- | Name of routine that forks procedures
fork :: CIdent
fork = "act_fork"

-- | Name of routine that initialized an activation record
act_enter :: CIdent
act_enter = "act_enter"

-- | Name of routine that deallocates an activation record
act_leave :: CIdent
act_leave = "act_leave"

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
unsched_event :: CIdent
unsched_event = "unsched_event"

-- | C type that represents model time
time_t :: C.Type
time_t = [cty|typename ssm_time_t |]

-- | C type that represents process priorities
priority_t :: C.Type
priority_t = [cty|typename priority_t|]

-- | C type that represents process depths
depth_t :: C.Type
depth_t = [cty|typename depth_t|]

-- | C type that represents triggers, aka processes that are sensitized on variables
trigger_t :: C.Type
trigger_t = [cty| struct trigger |]

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
act_t = [cty|struct act|]

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
svt_ ty = [cty|typename $id:(typeId ty ++ "_svt")|]

-- | Obtain the name of the initialize method for an SSM `Type`.
initialize_ :: Type -> CIdent
initialize_ ty = "initialize_" ++ typeId ty

-- | Obtain the name of the assign method for an SSM `Type`.
assign_ :: Type -> CIdent
assign_ ty = "assign_" ++ typeId ty

-- | Obtain the name of the later method for an SSM `Type`.
later_ :: Type -> CIdent
later_ ty = "later_" ++ typeId ty

{- | Given a reference and a list of local references, this function will return
a C expression that holds a pointer to the reference. -}
refPtr :: Reference -> [Reference] -> C.Exp
refPtr r@(Dynamic _) lrefs =
  if r `elem` lrefs
    then [cexp| &acts->$id:(refName r) |]
    else [cexp| acts->$id:(refName r)  |]
-- Static references can be referenced without an activation record
refPtr r@(Static _) _ = [cexp| &$id:(refName r) |]

{- | Given a Reference, this function will return a C expression that holds
the value of that reference. -}
refVal :: Reference -> [Reference] -> C.Exp
refVal r@(Dynamic _) lrefs =
  if r `elem` lrefs
    then [cexp| acts->$id:(refName r).value|]
    else [cexp| acts->$id:(refName r)->value|]
refVal r@(Static _) _      = [cexp| $id:(refName r).value |]
