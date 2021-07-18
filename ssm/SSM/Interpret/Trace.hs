{-| This module declares a datatype that is used to describe behavior of an SSM
program. The interpreter directly outputs elements of this type, while the
generated C code prints lines to the terminal representing the trace items
(parsed using the 'read').

When compiled for event-trace-based testing, the compiled code will include
statements that increment a microtick, to guard against divergent computation.
Since the possible sources of nontermination in SSM are unbounded recursion and
loops, these microticks will be placed at the beginning of each loop iteration,
and at the beginning of each step function. The running microtick count persists
between instants, so that it increases monotonically throughout the execution.
-}

module SSM.Interpret.Trace
  ( Trace(..)
  , Event(..)
  , isTerminal
  , isWellFormed
  ) where

import           Data.Word

-- -- | A typed, concrete value. TODO: decouple this from Literals.
-- type ConcreteValue = SSMLit

-- -- | A variable name.
-- type VarIdent = String

-- | The name of an activation record
type ActIdent = String

-- | What transpired during the execution of an SSM program.
type Trace = [Event]

-- | The events that characterize an execution of an SSM program.
data Event =
  -- | The length and head time of the event queue, reported between ticks.
    DriverEventQueueStatus Word Word64
  -- | Enter/re-enter a step function.
  | ActStepBegin ActIdent

{- TODO: the following, if they ever become necessary.

  -- | Leave a step function because the process terminated.
  | ActStepEndLeave ActIdent
  -- | Leave a step function because the process forked other processes.
  | ActStepEndFork ActIdent
  -- | Leave a step function because the process started waiting.
  | ActStepEndWait ActIdent
  -- | Value of variable in activation record.
  | ActLocalVal VarIdent ConcreteValue
  -- | Activate another process, in preparation for a fork.
  | ActActivate ActIdent [ConcreteValue]
  -- | Sensitize to a variable.
  | ActSensitize VarIdent

-}
  -- | Terminated gracefully.
  | TerminatedOk
  -- | Did not terminate within microtick limit.
  | ExhaustedMicrotick
  -- | Tried to queue too many activation records in an instant.
  | ExhaustedActQueue
  -- | Tried to schedule too many delayed assignments.
  | ExhaustedEventQueue
  -- | Tried to allocate too many activation records.
  | ExhaustedMemory
  -- | Tried to fork too deeply.
  | ExhaustedDepth
  -- | E.g., tried to schedule a "delayed" assignment for an earlier time.
  | CrashInvalidTime
  -- | Tried to compute invalid arithmetic exception.
  | CrashArithmeticError
  -- | Interpreter crashed for an unforeseen reason (should be unreachable).
  | CrashUnforeseen String
  deriving (Show, Eq, Read)

isTerminal :: Event -> Bool
isTerminal TerminatedOk         = True
isTerminal ExhaustedMicrotick   = True
isTerminal ExhaustedActQueue    = True
isTerminal ExhaustedEventQueue  = True
isTerminal ExhaustedMemory      = True
isTerminal ExhaustedDepth       = True
isTerminal CrashInvalidTime    = True
isTerminal CrashArithmeticError = True
isTerminal (CrashUnforeseen _)  = True
isTerminal _                    = False

isWellFormed :: Trace -> Bool
isWellFormed [] = False
isWellFormed es = isTerminal (last es) && not (any isTerminal (init es))
