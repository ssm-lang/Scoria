module Test.SSM.QuickCheck.Shrink.Statements
    ( statements ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

statements :: Program -> [Program]
statements = transformProcedures shrinkAllStmtsProcedure

-- | Return a list of new procedures where the procedure is mutated by removing
-- statements.
shrinkAllStmtsProcedure :: Procedure -> [Procedure]
shrinkAllStmtsProcedure p = [ p { body = bdy } 
                            | bdy <- shrinkBody (emptyHughes, body p)]

-- | Shrink the statements of a procedure body. SetRef, SetLocal, If, While, Skip,
-- After, Wait and Fork can be 'safely' removed, where safely means that the rest of
-- the program is still type safe.
shrinkBody :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkBody (_, [])          = []
shrinkBody (front, (x:xs)) = case x of
  SetRef _ _     -> emitPartial : continue
  SetLocal _ _ _ -> emitPartial : continue
  If c thn els   -> let thns   = shrinkBody (emptyHughes, thn)
                        elss   = shrinkBody (emptyHughes, els)
                        front' = fromHughes front
                        currth = [ front' ++ (If c thn' els : xs) | thn' <- thns]
                        currel = [ front' ++ (If c thn els' : xs) | els' <- elss]
                    in currth ++ (currel ++ continue)
  While c bdy    -> let bdys   = shrinkBody (emptyHughes, bdy)
                        front' = fromHughes front
                        curr   = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                    in curr ++ continue
  Skip           -> continue
  After _ _ _    -> emitPartial >> continue
  Wait _         -> emitPartial >> continue
  Fork _         -> emitPartial >> continue
  _              -> continue
  where
    emitPartial :: [Stm]
    emitPartial = fromHughes front ++ xs

    continue :: [[Stm]]
    continue = shrinkBody (snoc front x, xs)
