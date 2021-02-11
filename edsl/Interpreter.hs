{-# LANGUAGE GADTs #-}
module Interpreter where

import AST

import Control.Monad.State
import Data.List
import Data.IORef

import qualified Data.Map as Map

-- | Events to happen in the future
data Event = Event {
                     -- | The time when this event happens
                     time     :: Int
                     -- | The new value the variable will be given
                   , newValue :: SSMExp
                   }

-- | Activation record that holds all the information concerning a process
data ActivationRecord = AR {
                             -- | Priority of this AR
                             priority        :: Int
                             -- | Depth of this AR. Used together with the priority to give priorities to children.
                           , depth           :: Int
                             -- | The number of child processes that have yet not terminated
                           , runningChildren :: Int
                             -- | The step function, continuation, of this process
                           , continuation    :: SSM ()
                           }

-- | State that exists while we run the SSM computation
data St = St {
               now        :: Int
               -- | Map that associates a variable with when it was last updated
             , variables  :: Map.Map (IORef SSMExp) Int
               -- | Events. Use a map to ensure only one event exists per variable
             , events     :: Map.Map (IORef SSMExp) Event
               -- | Map that associates a variable to a list of ARs that are waiting on this variable
             , waiting    :: Map.Map (IORef SSMExp) [ActivationRecord]
               -- | Queue of processes ready to run. List for now, but should be changed to a priorityqueue later.
             , readyQueue :: [ActivationRecord]
               -- | Which procedure is currently running, if any
             , current    :: Maybe ActivationRecord
             }

type Eval a = StateT St IO a

{-
data SSM a where
    -- | Monadic operations
    Return  :: a -> SSM a
    Bind    :: SSM a -> (a -> SSM b) -> SSM b

    -- | Variable/Stream operations
    NewRef  :: String -> SSMExp -> SSM (String, (IORef SSMExp))
    SetRef  :: (String, IORef SSMExp) -> SSMExp -> SSM ()
    GetRef  :: (String, IORef SSMExp) -> SSM SSMExp
    
    -- | Control operations
    If      :: SSMExp -> SSM a -> Maybe (SSM a) -> SSM ()
    While   :: SSMExp -> SSM a -> SSM ()
    
    -- | SSM specific operations
    After   :: SSMExp -> (String, IORef SSMExp) -> SSMExp -> SSM ()
    Changed :: (String, IORef SSMExp) -> SSM SSMExp
    Wait    :: [(String, IORef SSMExp)] -> SSM ()
    Fork    :: [SSM ()] -> SSM ()

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> SSM (a -> b)
    Argument  :: String -> Either SSMExp (String, IORef SSMExp) -> SSM ()
    Result    :: Res a => String -> a -> SSM ()
-}

evalSSM :: SSM a -> Eval a
evalSSM (Return a)   = return a
--evalSSM (Bind ma fa) = undefined

--evalSSM (NewRef n v) = do ref <- liftIO $ newIORef v
--                          undefined

tick :: Eval ()
tick = do
    modify $ \st -> st { now = now st + 1}
    done <- doneExecuting
    if done
        then return ()
        else do
             st <- get
             let (currentEvents, futureEvents) = Map.partition (\(Event t' _) -> t' == now st) $ events st
             put $ st { events = futureEvents}
             mapM performEvent $ Map.toList currentEvents
             schedule
             tick
  where
      performEvent :: (IORef SSMExp, Event) -> Eval ()
      performEvent (r, (Event _ v)) = do
          liftIO $ writeIORef r v
          -- check which threads should be awakened by this write.
          -- NOTE: also remember to remove them from any other variables they might be waiting for

      doneExecuting :: Eval Bool
      doneExecuting = do st <- get
                         return $ null (events st) && null (readyQueue st)

schedule :: Eval ()
schedule = do
    st <- get
    case readyQueue st of
        []     -> return ()
        (x:xs) -> do put (st { readyQueue = xs, current = Just x})
                     evalSSM (continuation x)
                     schedule

runSSM :: SSM () -> IO ()
runSSM ma = evalStateT (evalSSM ma) st
  where
      st = St 0 Map.empty Map.empty Map.empty [AR 0 1024 0 ma] Nothing