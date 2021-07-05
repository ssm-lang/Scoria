module Test.SSM.QuickCheck.Shrink.If
    ( ifs ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

ifs :: Program -> [Program]
ifs = transformProcedures shrinkIfProcedure

shrinkIfProcedure :: Procedure -> [Procedure]
shrinkIfProcedure p = let bodys = shrinkIfStm (emptyHughes, body p)
                      in for bodys $ \bdy -> p { body = bdy }

shrinkIfStm :: (Hughes Stm,[Stm]) -> [[Stm]]
shrinkIfStm (_,[])           = []
shrinkIfStm (front, (x:xs)) = case x of
  If c thn els -> let front' = fromHughes front
                      curr   = [front' ++ thn ++ xs, front' ++ els ++ xs]
                  in curr  ++ shrinkIfStm (snoc front x, xs)
  While c bdy -> let bdys  = shrinkIfStm (emptyHughes, bdy)
                     front' = fromHughes front
                     curr = [ front' ++ (While c bdy' : xs) | bdy' <- bdys]
                 in curr ++ shrinkIfStm (snoc front x, xs)
  _ -> shrinkIfStm (snoc front x, xs)
