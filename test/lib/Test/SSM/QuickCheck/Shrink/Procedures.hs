module Test.SSM.QuickCheck.Shrink.Procedures
    ( shrinkSingleProcedures
    , shrinkManyProcedures
    , removeUnusedProcedures
    ) where

import SSM.Core.Syntax

import Test.SSM.QuickCheck.Util

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

{-***** Removing entire procedures *****-}

-- | Return all mutations where one function were removed from the program. Never
-- tries to remove the main function.
shrinkSingleProcedures :: Program -> [Program]
shrinkSingleProcedures p =
  -- for each procedure in the program that is not the programs entry point
  let ps = for (delete (entry p) (Map.keys (funs p))) $ \fun ->
        -- delete it from the program
        let p' = p { funs = Map.delete fun (funs p) }
        -- and then remove any reference to it in the rest of the program
        in removeProcedure p' [fun]
      
      ps' = filter isJust ps
  in map fromJust ps'
  
{- | A more greedy version of program shrinking where one third of the entire program
is removed at once. -} 
shrinkManyProcedures :: Program -> [Program]
shrinkManyProcedures p =
  concat $ for [ removeProcedure p h1
               , removeProcedure p h2
               , removeProcedure p h3
               ] $ \mp ->
  maybe [] (: []) mp

  where

    -- | Procedures that can be removed (all except the programs entry point)
    toremove :: [String]
    toremove = delete (entry p) (Map.keys (funs p))

    {- | each of h1, h2 & h3 contains the names of a third of the programs procedures
    each. -}
    h1,h2,h3 :: [String]
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

{- | Takes a program and a list of names of procedures that are no longer existing.
Returns a new program (if successful) where there are no longer any references to
the procedures that are deleted. -}
removeProcedure :: Program -> [String] -> Maybe Program
removeProcedure p procs = do
  funs' <- removeFromProcedures (Map.toList (funs p)) procs False
  return $ p { funs = Map.fromList funs' }

removeFromProcedures :: [(String,Procedure)]
                     -> [String]
                     -> Bool
                     -> Maybe [(String,Procedure)]
removeFromProcedures [] _ b             = if b then Just [] else Nothing
removeFromProcedures ((n,p):ps) procs b =
  if n `elem` procs
    then removeFromProcedures ps procs True
    else case remove p procs of
      Just p' -> do ps' <- removeFromProcedures ps procs (b || True)
                    return $ (n,p') : ps'
      Nothing -> do ps' <- removeFromProcedures ps procs (b || False)
                    return $ (n,p) : ps'

remove :: Procedure -> [String] -> Maybe Procedure
remove p funs = case newbody (body p, False) of
  (_, False)  -> Nothing
  (bdy, True) -> Just $ p { body = bdy }
  where
    newbody :: ([Stm], Bool) -> ([Stm], Bool)
    newbody ([], b)     = ([], b)
    newbody ((x:xs), b) = case x of
      If c thn els ->
        let (thn',b1) = newbody (thn, b)
            (els',b2) = newbody (els, b1)
            stm       = If c thn' els'
            (xs',b3)  = newbody (xs, b2)
        in (stm:xs', b3)

      While c bdy  ->
        let (bdy',b1) = newbody (bdy, b)
            stm       = While c bdy'
            (xs',b2)  = newbody (xs, b1)
        in (stm : xs', b2)
      
      Fork procs   -> do
        let procs' = removeFork procs funs
        case procs' of
          Just []      -> newbody (xs, b || True)
          Just procs'' -> let (xs',b1) = newbody (xs, b || True)
                          in (Fork procs'' : xs', b1)
          Nothing      -> let (xs',b1) = newbody (xs, b || False)
                          in (Fork procs : xs', b1)

      otherwise    ->
        let (xs',b1) = newbody (xs, b)
        in (otherwise : xs', b1)
    
    removeFork :: [(String, [Either SSMExp Reference])]
               -> [String]
               -> Maybe [(String, [Either SSMExp Reference])]
    removeFork procs funs = go procs funs False
      where
        go [] _ b                 = if b then Just [] else Nothing
        go (x@(n,args):xs) funs b = do
          if n `elem` funs
            then do go xs funs (b || True)
            else do xs' <- go xs funs (b || False)
                    return $ x : xs'

removeUnusedProcedures :: Program -> Program
removeUnusedProcedures p = case removeProcedure p (toremove' p) of
  Just p -> p
  Nothing -> p

usedInStm :: [Stm] -> Set.Set String
usedInStm [] = Set.empty
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

toremove' :: Program -> [String]
toremove' p = let s1 = Set.fromList $ Map.keys (funs p)
                  s2 = usedInStm $ body (fromJust (Map.lookup (entry p) (funs p)))
                  s3 = Set.union s2 (Set.singleton (entry p))
              in Set.toList $ s1 `Set.difference` s3
