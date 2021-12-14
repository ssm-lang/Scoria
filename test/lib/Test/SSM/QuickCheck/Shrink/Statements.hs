module Test.SSM.QuickCheck.Shrink.Statements
    ( statements ) where

import SSM.Core.Syntax
import SSM.Core.Program

import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

statements :: Program backend -> [Program backend]
statements = transformProcedures shrinkAllStmtsProcedure

-- | Return a list of new procedures where the procedure is mutated by removing
-- statements.
shrinkAllStmtsProcedure :: Procedure -> [Procedure]
shrinkAllStmtsProcedure p =
  [ p { body = body' } | body' <- distributeMutate (body p) shrinkStmts ]

-- | Replace any of the below statements with a skip instruction
shrinkStmts :: Stm -> [Stm]
shrinkStmts stm = case stm of
  SetRef _ _     -> [Skip]
  SetLocal _ _ _ -> [Skip]
  After _ _ _    -> [Skip]
  Wait _         -> [Skip]
  Fork _         -> [Skip]
  _              -> []
