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
{-# LANGUAGE OverloadedStrings #-}
module SSM.Interpret.Trace where

import qualified Data.Text                     as T
import           Data.Word

import           SSM.Core.Syntax
import           SSM.Core.Type

import           Test.QuickCheck

-- | What transpired during the execution of an SSM program.
type Trace = [Event]

-- | The events that characterize an execution of an SSM program.
data Event =
  -- | The length and head time of the event queue, reported between ticks.
    DriverEventQueueStatus Word Word64

  -- | Enter/re-enter a step function.
  | ActStepBegin ActIdent

  -- | Value of initialized variable (arguments + locals) in activation record.
  --
  -- Since each activation record maintains multiple variables, it is important
  -- to discuss the expected order of these events here. Arguments appear first,
  -- sorted by name, followed by local variables, also sorted by name. Any
  -- uninitialized variable should not be reported.
  --
  -- ActVar events should appear after ActStepBegin, but before any other
  -- computation in the step function (including the microtick).
  | ActVar VarVal

  -- | Activate another process, in preparation for a fork.
  --
  -- If forking multiple processes, these events should appear in fork order.
  | ActActivate ActIdent

  -- | Sensitize to a variable.
  --
  -- If sensitizing on multiple variables, these should appear in wait order
  -- (although this does not have any operational significance).
  | ActSensitize VarIdent

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
  | ExhaustedPriority
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
isTerminal ExhaustedPriority    = True
isTerminal CrashInvalidTime     = True
isTerminal CrashArithmeticError = True
isTerminal (CrashUnforeseen _)  = True
isTerminal _                    = False

isWellFormed :: Trace -> Bool
isWellFormed [] = False
isWellFormed es = isTerminal (last es) && not (any isTerminal (init es))

-- | A variable name.
type VarIdent = String

-- | The name of an activation record.
type ActIdent = String

-- | The trace representation indicating a variable, its type, and its value.
--
-- Even if the variable is a reference, VarVal should contain its base type
-- (i.e., without the reference) and base value (i.e., dereferenced).
data VarVal = VarVal VarIdent Type ConcreteValue
  deriving (Show, Eq, Read)

-- | An untyped, concrete value.
--
-- To make things easier for the parser, we use a representation that is more
-- or less agnostic to the type of the value, i.e., integers.
data ConcreteValue =
    -- | A value represented as an integer.
    --
    -- Encompasses all Int and UInt types, as well as Bool {0,1}.
    --
    -- Use uninitalizedMagicIntegral to represent an uninitialized value.
      IntegralVal Integer

    -- | A placeholder "value"; should not appear in the concrete event trace.
    --
    -- Used for injecting formatters into a format string in codegen.
    | IntegralFmt String

    -- | A "value" that inhabits a singleton type (such as Event).
    | UnitType
    deriving (Eq, Read)

-- | Override the default Show intance for @ConcreteVal@ so that shown
-- formatters can be parsed as values.
instance Show ConcreteValue where
  show (IntegralVal i) = "(IntegralVal " ++ show i ++ ")"
  show (IntegralFmt f) = "(IntegralVal " ++ f ++ ")"
  show UnitType        = "UnitType"

actStepBegin :: T.Text
actStepBegin = "ActStepBegin"

actActivate :: T.Text
actActivate = "ActActivate"

varVal :: T.Text
varVal = "VarVal"

actVar :: T.Text
actVar = "ActVar"

driverEventQueueStatus :: T.Text
driverEventQueueStatus = "DriverEventQueueStatus"

actSensitize :: T.Text
actSensitize = "ActSensitize"

terminatedOk :: T.Text
terminatedOk = "TerminatedOk"

exhaustedMicrotick :: T.Text
exhaustedMicrotick = "ExhaustedMicrotick"

exhaustedActQueue :: T.Text
exhaustedActQueue = "ExhaustedActQueue"

exhaustedEventQueue :: T.Text
exhaustedEventQueue = "ExhaustedEventQueue"

exhaustedMemory :: T.Text
exhaustedMemory = "ExhaustedMemory"

exhaustedPriority :: T.Text
exhaustedPriority = "ExhaustedPriority"

crashInvalidTime :: T.Text
crashInvalidTime = "CrashInvalidTime"

crashArithmeticError :: T.Text
crashArithmeticError = "CrashArithmeticError"

crashUnforeseen :: T.Text
crashUnforeseen = "CrashUnforeseen"

integralVal :: T.Text
integralVal = "IntegralVal"

unitType :: T.Text
unitType = "UnitType"

ref :: T.Text
ref = "Ref"

{-********** Generate random traces **********-}

instance Arbitrary Type where
  arbitrary = elements $ basetypes ++ Prelude.map Ref basetypes
    where basetypes = [TUInt8, TUInt64, TInt32, TInt64, TBool, TEvent]

instance Arbitrary ConcreteValue where
  arbitrary = oneof [IntegralVal <$> arbitrary, return UnitType]

instance Arbitrary VarVal where
  arbitrary = do
    i <- arbitrary :: Gen Word8
    let varIdent = "v" ++ show i
    t  <- arbitrary
    cv <- arbitrary
    return $ VarVal varIdent t cv

instance Arbitrary Event where
  arbitrary = oneof
    [ return ExhaustedActQueue
    , return ExhaustedEventQueue
    , return ExhaustedMicrotick
    , return ExhaustedPriority
    , return ExhaustedMemory
    , return CrashArithmeticError
    , return CrashInvalidTime
    , do
      reason <- oneof [return "error1", return "error2", return "error3"]
      return $ CrashUnforeseen reason
    , return TerminatedOk
    , ActSensitize <$> arbVar
    , ActActivate <$> arbAct
    , ActVar <$> arbitrary
    , ActStepBegin <$> arbAct
    , do
      i <- arbitrary
      t <- arbitrary
      return $ DriverEventQueueStatus i t
    ]
   where
    arbVar :: Gen String
    arbVar = do
      i <- arbitrary :: Gen Word8
      return $ "v" ++ show i

    arbAct :: Gen String
    arbAct = do
      i <- arbitrary :: Gen Word8
      return $ "fun" ++ show i
