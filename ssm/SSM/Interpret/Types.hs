{- | This module declares the different types used by the interpreter to interpret a
SSM program, represented in the "SSM.Core.Syntax" format. While the interpreter is
meant to evaluate programs the same way the runtime system does, the model used here
is slightly simpler. Some of the definitions are similar but not identical to the
corresponding RTS ones. -}
{-# LANGUAGE StrictData #-}
module SSM.Interpret.Types
  ( -- * Types
      -- ** Schedulable variables
      {- | A schedulable variable is just a fancy name for the normal variables we
      have in SSM programs. They can receive immediate assignment etc, but they can
      also be scheduled to receive an update in the future. -}
    Var
      -- ** Process activation records
      {- | The process activation record very closely resembles the corresponding C
      version, but jas a few differences. -}
  , Proc(..)
      -- ** Interpretation monad
      {- | This is interpretation monad that we use to interpret a program. It is a
      state+writer monad. -}
  , Interp
      -- ** State maintained by the interpretation monad
      {- | The main interpretation state. This state is passed around throughout all
      the interpretation, being constantly modified. There might be some room for
      improving efficiency here, but by profiling the interpreter I don't see any
      immediate condidates. -}
  , St(..)

      -- ** Configuration object used to interpret programs
  , InterpretConfig(..)

      -- * Utility functions
  , mkProc
  , variableStorage
  , interpState
  , lift'
  , crash
  , terminate
  , tellEvent
  ) where

import           Data.List
import           Data.Maybe
import           Data.STRef.Lazy
import           Data.Word

import           SSM.Core.Syntax
import           SSM.Util.HughesList

import           Control.Monad.ST.Lazy
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy

import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map
import qualified SSM.Interpret.Trace           as T

{- | SSM interpreter variables. A variable is a reference to a 5-tuple.
The components are

@
  ( Reference to the actual value of the variable
  , List of processes that are waiting for writes to this variable
  , The time when this variable was last written to
  , The next time there's a scheduled event on this variable
  , The new value this variable will get at the next event time
  )
@
-}
type Var s
  = STRef
      s
      ( -- Reference pointing to the actual value of the variable.
       STRef s SSMExp, -- List of processes that are waiting for writes to this variable.
                       Map.Map Int (Proc s), -- The time when this variable was last written to
                                             Word64
    -- The next time there's a scheduled event on this variable
                                                   , Maybe Word64
    -- The new value this variable will get at the next event time
                                                                 , Maybe SSMExp)

-- | Process activation records.
data Proc s = Proc
  { -- | Name of the process
    procname :: String
    -- | priority of the process
  , priority        :: Int
    -- | The depth, which helps give priorities to children
  , depth           :: Int
    -- | Number of alive child processes
  , runningChildren :: Int
    {- | Parent of this process, which is @Nothing@ if the current process is the
    programs entrypoint. Otherwise, this field is always a @Just -something@. -}
  , parent          :: Maybe (STRef s (Proc s))
    {- | This is a variable storage. The variables found in this map are the
    expressions or variables that are passed to the process as arguments from
    a parent. -}
  , variables       :: Map.Map String (Var s)
    {- | Variables found in this map are those that are local to this process, aka
    those that are created by the `SSM.Core.Syntax.NewRef` constructor. We need to
    put them in a separate map so that we can quickly deschedule any outstanding events
    on them when a process is terminating. -}
  , localrefs       :: Map.Map String (Var s)
    -- | Variables this process is waiting for, if any
  , waitingOn       :: Maybe [Var s]
    -- | The work left to do for this process
  , continuation    :: [Stm]
  }
  deriving Eq

{- | The show instance will only render the priority (this was initially
implemented for debug purposes). -}
instance Show (Proc s) where
  show p = show $ priority p

-- | Ordering is computed by comparing priorities.
instance Ord (Proc s) where
  p1 <= p2 = priority p1 <= priority p2

-- | The interpreter state maintained while interpreting a program.
data St s = St
  { -- | The current model time
    now               :: Word64
    {- | The outstanding events. Represented as a map from the time at which the
    event should occur to a list of the variables that should be updated at that time.
    This representation is faithful to the order in which the events are inserted in
    the event queue, while the C heap might shuffle events around when they are
    scheduled for the same instant. -}
  , events            :: Map.Map Word64 [Var s]
    {- | Number of outstanding events. Bounded by the number of variables currently
    allocated in the program, as there can be at most 1 outstanding event per
    variable. While this number could be derived from the map, that would be a linear
    complexity computation. If we maintain this state we can look it up in @O(1)@. -}
  , numevents         :: Int
    {- | Map that associates priorities to processes. We use a map as it gives us a
    pleasant complexity for getting the minimum element. This map only holds processes
    that are scheduled to be evaluated. -}
  , readyQueue        :: IntMap.IntMap (Proc s)
    -- | Number of processes in the readyqueue
  , numconts          :: Int
    {- | Map that associates procedure names with procedure definitions. Used when we
    fork a procedure and we need to create an activation record. -}
  , procedures        :: Map.Map String Procedure
    {- | Argment-references given to the entrypoint. Will probably be removed, depending
    on how we end up managing input/output in SSM programs. -}
  , inputargs         :: [(String, Var s)]
    -- | The process that is currently being evaluated
  , process           :: Proc s
  , maxContQueueSize  :: Int
  , maxEventQueueSize :: Int
  }
  deriving Eq

{- | Alias for creating a process, so we don't need to rely on the internals of the
@Proc s@ datatype. -}
mkProc
  :: String                    -- ^ Name
  -> Int                       -- ^ Priority
  -> Int                       -- ^ Depth
  -> Int                       -- ^ #Running children
  -> Maybe (STRef s (Proc s))  -- ^ Reference to parent, if any
  -> Map.Map String (Var s)    -- ^ Variable storage
  -> Map.Map String (Var s)    -- ^ Local reference storage
  -> Maybe [Var s]             -- ^ Variables to wait for
  -> [Stm]                     -- ^ Continuation
  -> Proc s
mkProc = Proc

{- | Alias for getting the variable storage from a process, so we don't expose the
internals of the @Proc s@ datatype. -}
variableStorage :: Proc s -> Map.Map String (Var s)
variableStorage = variables

{- | Alias for creating the interpretation state, so that we don't expose the internals
of the @St@ type to the developer. -}
interpState
  :: Word64                    -- ^ Now
  -> Map.Map Word64 [Var s]    -- ^ Events
  -> Int                       -- ^ #numevents
  -> IntMap.IntMap (Proc s)    -- ^ Ready queue
  -> Int                       -- ^ #numcontinuations
  -> Map.Map String Procedure  -- ^ Procedures
  -> [(String, Var s)]         -- ^ Input references
  -> Proc s                    -- ^ Current process
  -> Int                       -- ^ Max continuation queue size
  -> Int                       -- ^ Max event queue size
  -> St s
interpState = St

-- | Interpretation monad
type Interp s a = StateT (St s) (WriterT (Hughes T.Event) (ST s)) a

-- | Lift a @ST@ computation to the interpretation monad.
lift' :: ST s a -> Interp s a
lift' = lift . lift

-- | Emit event to event log in the interpretation monad.
tellEvent :: [T.Event] -> Interp s ()
tellEvent = tell . toHughes

-- | Halt interpretation monad, and report terminal condition.
terminate :: T.Terminal -> Interp s a
terminate t = error $ show t

-- | Crash upon unforeseen failure mode (as reported by string).
crash :: String -> Interp s a
crash = terminate . T.CrashUnforeseen

{- | Data type of interpreter configuration. Need to modify the interpreter to
interpret a program after loading this information into the interpretation state.
I can hack this together on monday. -}
data InterpretConfig = InterpretConfig
  { -- | Size of continuation queue
    boundContQueueSize  :: Int
                       -- | Size of event queue
  , boundEventQueueSize :: Int
                       -- | Program to interpret
  , program             :: Program
  }
