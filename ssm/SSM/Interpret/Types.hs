module SSM.Interpret.Types
    ( -- * Types
      -- ** Schedulable variables
      Var
      -- ** Process activation records
    , Proc(..)
      -- ** Interpretation monad
    , Interp
      -- ** State maintained by the interpretation monad
    , St(..)

      -- * Utility functions
    , mkProc
    , variableStorage
    , interpState
    , lift'
    ) where

import Data.STRef.Lazy
import Data.Word
import Data.Maybe
import Data.List

import SSM.Core.Syntax
import SSM.Util.HughesList

import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified SSM.Interpret.Trace as T

type Var s = STRef s
  ( -- Reference pointing to the actual value of the variable.
    STRef s SSMExp
  , -- List of processes that are waiting for writes to this variable.
    Map.Map Int (Proc s)
  , -- The time when this variable was last written to
    Word64
    -- The next time there's a scheduled event on this variable
  , Maybe Word64
    -- The new value this variable will get at the next event time
  , Maybe SSMExp
  )

-- | State of a single process. Equivalent to the struct in C.
data Proc s = Proc
  { -- | priority of the process
    priority        :: Int
    -- | The depth, which helps give priorities to children
  , depth           :: Int
  -- | Number of non-terminated child processes
  , runningChildren :: Int
  -- | Parent of this process, Nothing in the case of main
  , parent          :: Maybe (STRef s (Proc s))
  -- | Variables that are either expressions or references passed from a parent
  , variables       :: Map.Map String (Var s)
  -- | Variables that are references that are local to this process
  , localrefs       :: Map.Map String (Var s)
  -- | Variables this process is waiting for, if any
  , waitingOn       :: Maybe [Var s]
  -- | The work left to do for this process
  , continuation    :: [Stm]
  }
  deriving Eq

instance Show (Proc s) where
    show p = show $ priority p

instance Ord (Proc s) where
    p1 <= p2 = priority p1 <= priority p2

-- | The interpreter state.
data St s = St
  { -- | Current time
    now        :: Word64
  -- | Outstanding events
  , events     :: Map.Map Word64 [Var s]
  -- | Number of outstanding events
  , numevents  :: Int
  -- | Processes ready to run, should be a priority queue
  , readyQueue :: IntMap.IntMap (Proc s)
  -- | Number of processes in the readyqueue
  , numconts   :: Int
  -- | Map that associated names with procedures.
  , procedures :: Map.Map String Procedure
  -- | Argment-references given to the entrypoint
  , inputargs  :: [(String, Var s)]
  -- | Currently running process
  , process    :: Proc s
  }
  deriving Eq

{- | Alias for creating a process, so we don't expose the internals of the Proc s
datatype. -}
mkProc :: Int                       -- ^ Priority
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
internals of the Proc s datatype. -}
variableStorage :: Proc s -> Map.Map String (Var s)
variableStorage = variables

{- | Alias for creating the interpretation state, so that we don't expose the internals
of the St type to the developer. -}
interpState :: Word64                    -- ^ Now
            -> Map.Map Word64 [Var s]    -- ^ Events
            -> Int                       -- ^ #numevents
            -> IntMap.IntMap (Proc s)    -- ^ Ready queue
            -> Int                       -- ^ #numcontinuations
            -> Map.Map String Procedure  -- ^ Procedures
            -> [(String, Var s)]         -- ^ Input references
            -> Proc s                    -- ^ Current process
            -> St s
interpState = St

type Interp s a = StateT (St s) (WriterT (Hughes T.OutputEntry) (ST s)) a

-- | Lift a ST computation to the interpretation monad.
lift' :: ST s a -> Interp s a
lift' = lift . lift