module Test.SSM.QuickCheck.Shrink.Procedures
    ( shrinkSingleProcedures
    , shrinkManyProcedures
    , removeUnusedProcedures
    ) where

import SSM.Core.Syntax
import SSM.Core.Ident
import SSM.Core.Program

import Test.SSM.QuickCheck.Util

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Return all mutations where one function were removed from the program. Never
-- tries to remove the main function.
shrinkSingleProcedures :: Program -> [Program]
shrinkSingleProcedures p =
  -- for each procedure in the program that is not the programs entry point
  let ps = for (delete (entry p) (Map.keys (funs p))) $ \fun ->
             removeCallsFromProgram [fun] p
      
      ps' = filter isJust ps
  in map fromJust ps'

{- | A more greedy versio pn of program shrinking where one third of the entire program
is removed at once. -} 
shrinkManyProcedures :: Program -> [Program]
shrinkManyProcedures p = concat $ for [ removeCallsFromProgram h1 p
                                      , removeCallsFromProgram h2 p
                                      , removeCallsFromProgram h3 p
                                      ] $ maybe [] (: [])

  where

    -- | Procedures that can be removed (all except the programs entry point)
    toremove :: [Ident]
    toremove = delete (entry p) (Map.keys (funs p))

    {- | each of h1, h2 & h3 contains the names of a third of the programs procedures
    each. -}
    h1,h2,h3 :: [Ident]
    (h1,h2,h3) = let l       = length toremove `div` 3
                     (h1,r)  = mysplit l toremove
                     (h2,h3) = mysplit l r
                  in (h1,h2,h3)

    {- | Takes a length @i@ and a list @xs@ and return two lists, where the first list
    is the first @i@ elements from @xs@ and the second list is the remainder of @xs@
    after removing the first @i@ elements. -}
    mysplit :: Int -> [a] -> ([a],[a])
    mysplit i xs = go i ([], xs)
      where
        go :: Int -> ([a],[a]) -> ([a],[a])
        go 0 (sx,ys)   = (reverse sx, ys)
        go i (sx, [])  = (reverse sx, [])
        go i (sx,y:ys) = go (i-1) (y : sx, ys)


{- | Take a list of names of procedures to remove and a program and return the same
program but where now no procedure every forks any of the deleted procedures.
Returns @Nothing@ if the program was unchanged (in which case no shrinking could
have occured). -}
removeCallsFromProgram :: [Ident] -> Program -> Maybe Program
removeCallsFromProgram deletedfuns p =
  let -- delete the procedures from the program
      funs'   = foldl (\m' n -> Map.delete n m') (funs p) deletedfuns
      -- remove any calls to the deleted procedures
      funs''  = map (removeCallsFromProcedure deletedfuns) (Map.elems funs')
      funs''' = map (\p -> (name p, p)) funs''

      -- replace the old procedures with the new procedures
      p'      = p { funs = Map.fromList $ funs''' }

  -- if the initial program and the shrunk program are the same, then shrinking did
  -- not work and we should return Nothing.
  in if p == p'
      then Nothing
      else Just $ p { funs = Map.fromList $ funs''' }

{- | Take a list of procedures that are deleted and a procedure, and return a new
procedure where any call to the deleted procedures have been deleted. -}
removeCallsFromProcedure :: [Ident] -> Procedure -> Procedure
removeCallsFromProcedure funs p =
  let body' = removeCallsFromStmts (body p)
  in p { body = body' }
  where

    -- | Traverse a procedure body and remove any calls to a removed procedure.
    removeCallsFromStmts :: [Stm] -> [Stm]
    removeCallsFromStmts []     = []
    removeCallsFromStmts (x:xs) = case x of
      If c thn els ->
        If c (removeCallsFromStmts thn) (removeCallsFromStmts els)
        : removeCallsFromStmts xs
      While c bdy  ->
        While c (removeCallsFromStmts bdy)
        : removeCallsFromStmts xs
      Fork procs   -> let procs' = filter (\(n,_) -> not (n `elem` funs)) procs
                          xs'    = removeCallsFromStmts xs
                      in if null procs' then xs' else Fork procs' : xs'
      _            -> x : removeCallsFromStmts xs













-- | Remove procedures from a program that are not used (dead code)
removeUnusedProcedures :: Program -> Program
removeUnusedProcedures p = case removeCallsFromProgram (toremove' p) p of
  Just p -> p
  Nothing -> p
  where
     {- | Traverses a program and builds a set of all the names of procedures
     that are used. Procedures can only be references from `Fork` statements. -}
     usedInStm :: [Stm] -> Set.Set Ident
     usedInStm []     = Set.empty
     usedInStm (x:xs) = case x of
       If c thn els -> let s1 = usedInStm thn
                           s2 = usedInStm els
                           s3 = usedInStm xs
                       in Set.unions [s1,s2,s3]

       While c bdy -> let s1 = usedInStm bdy
                          s2 = usedInStm xs
                      in Set.union s1 s2

       Fork procs -> let s1 = Set.fromList $ map fst procs
                         s2 = usedInStm xs
                     in Set.union s1 s2

       otherwise -> usedInStm xs

     {- | Takes a program and returns a list of names of procedures that are unused can that
     can be safely removed all together. -}
     toremove' :: Program -> [Ident]
     toremove' p = let -- Set of procedures that exist
                       s1 = Set.fromList $ Map.keys (funs p)
                       -- Set of procedures that are used
                       s2 = usedInStm $ body (fromJust (Map.lookup (entry p) (funs p)))
                       -- Procedures to keep
                       s3 = Set.union s2 (Set.singleton (entry p))
                   -- Procedures to remove are those that are not in the @s3@ Set.
                   in Set.toList $ s1 `Set.difference` s3
