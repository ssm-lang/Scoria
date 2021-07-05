module Test.SSM.QuickCheck.Shrink.Wait
    ( waits ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

import Data.List

waits :: Program -> [Program]
waits = transformProcedures shrinkWaitProcedure

shrinkWaitProcedure :: Procedure -> [Procedure]
shrinkWaitProcedure p = let bodys = shrinkWaitStm (emptyHughes, body p)
                        in for bodys $ \bdy -> p { body = bdy }

shrinkWaitStm :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkWaitStm (_, [])          = []
shrinkWaitStm (front, (x:xs)) = case x of
  While c bdy -> let bdys   = shrinkWaitStm (emptyHughes, bdy)
                     front' = fromHughes front
                     currs  = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                 in currs ++ shrinkWaitStm (snoc front x, xs)
  
  Wait refs -> let sublists = filter (not . null) $ map (\r -> delete r refs) refs
                   front'   = fromHughes front
                   currs    = [ front' ++ (Wait l : xs) | l <- sublists]
               in currs ++ shrinkWaitStm (snoc front x, xs)
 
  _ -> shrinkWaitStm (snoc front x, xs)
