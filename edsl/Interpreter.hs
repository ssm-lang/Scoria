module Interpreter where

import AST

import Data.IORef

--import qualified Data.Set as Set

-- | Which variables have been written to in this current instant?
--type Written = Set.Set IORef

data ActivationRecord = AR { priority        :: Int
                           , depth           :: Int
                           , runningChildren :: Int
                           , caller          :: IORef ActivationRecord
                           , continuation    :: SSM ()
                           }