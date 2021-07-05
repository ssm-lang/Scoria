module Test.SSM.QuickCheck.Shrink.Fork
    ( forks ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

import Data.List

forks :: Program -> [Program]
forks = transformProcedures shrinkForksProcedure

shrinkForksProcedure :: Procedure -> [Procedure]
shrinkForksProcedure p = let bdys = shrinkForkStm (emptyHughes, body p)
                         in map (\bdy -> p { body = bdy } ) bdys

shrinkForkStm :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkForkStm (_, [])          = []
shrinkForkStm (front, (x:xs)) = case x of
  While c bdy  -> let bdys   = shrinkForkStm (emptyHughes, bdy)
                      front' = fromHughes front
                      curr   = [ (front' ++ (While c bdy' : xs)) | bdy' <- bdys]
                  in curr ++ shrinkForkStm (snoc front x, xs)

  Fork procs   ->
    let procss = filter (not . null) $ map (\f -> delete f procs) procs
        front' = fromHughes front
        curr   = [ front' ++ (Fork ps : xs) | ps <- procss]
    in curr ++ shrinkForkStm (snoc front x, xs)

  _ -> shrinkForkStm (snoc front x, xs)
