module Test.SSM.QuickCheck.Shrink.Fork
    ( forks ) where

import SSM.Core.Syntax
import SSM.Core.Program

import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

import Data.List

{- | Shrink fork statements in a program. Each resulting program contains only one
mutation. -}
forks :: Program -> [Program]
forks = transformProcedures shrinkForksProcedure

{- | Shrink all fork statements found in a procedure. Every procedure in the output
list contains at most and exactly one mutation. -}
shrinkForksProcedure :: Procedure -> [Procedure]
shrinkForksProcedure p =
  [ p { body = body' } | body' <- distributeMutate (body p) shrinkForkStm ]

{- | Shrink fork statements by replacing the list of forked processes with all
sublists of length @length procs - 1@. -}
shrinkForkStm :: Stm -> [Stm]
shrinkForkStm stm = case stm of
  Fork procs -> let sublists         = map (\f -> delete f procs) procs
                    nonemptysublists = filter (not . null) sublists
                in map Fork nonemptysublists
  _ -> []
