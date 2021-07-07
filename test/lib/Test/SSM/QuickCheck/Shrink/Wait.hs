module Test.SSM.QuickCheck.Shrink.Wait
    ( waits ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

import Data.List

import Debug.Trace

waits :: Program -> [Program]
waits = transformProcedures shrinkWaitProcedure

shrinkWaitProcedure :: Procedure -> [Procedure]
shrinkWaitProcedure p =
    [ p { body = body' } | body' <- distributeMutate (body p) shrinkWaits ]

shrinkWaits :: Stm -> [Stm]
shrinkWaits stm = case stm of
    Wait refs -> let sublists         = map (\r -> delete r refs) refs
                     nonemptysublists = filter (not . null) sublists
                 in map Wait nonemptysublists
    _ -> []
