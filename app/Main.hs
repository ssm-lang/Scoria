{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List

import Frontend
import Fib
import NonTerminate
import Core ()
import LowCore hiding (main)
import qualified Data.Map as Map
import qualified LowCore as LC
import qualified LowInterpreter as I1
import Data.Time ( diffUTCTime, getCurrentTime )
import System.IO
import Criterion.Main

singlecase :: Program
singlecase = Program {LC.main = "fun2", args = [Right ("ref3",Ref TUInt64)], funs = Map.fromList [("fun2",Procedure {name = "fun2", arguments = [("ref3",Ref TUInt64)], body = [After (Lit TUInt64 (LUInt64 2657)) ("ref3",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 28320)) (Lit TUInt64 (LUInt64 7172)) OPlus) (BOp TUInt64 (Lit TUInt64 (LUInt64 45518)) (Lit TUInt64 (LUInt64 762)) OTimes) OTimes),Wait [("ref3",Ref TUInt64)],Fork [("fun2",[Right ("ref3",Ref TUInt64)]),("fun2",[Right ("ref3",Ref TUInt64)]),("fun2",[Right ("ref3",Ref TUInt64)])],After (Lit TUInt64 (LUInt64 3429)) ("ref3",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 3880)) (Lit TUInt64 (LUInt64 36812)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 10144)) (Lit TUInt64 (LUInt64 16298)) OPlus) OPlus),Wait [("ref3",Ref TUInt64)],Fork [("fun2",[Right ("ref3",Ref TUInt64)])]]})]}

main :: IO ()
main = do
  let tr = take 10000 $ I1.interpret singlecase
  if tr == (tr ++ []) then return () else return ()
  --putStrLn $ unlines $ map show $ tr
  
--  defaultMain
--    [ bench "old interpreter" $ nf (\c -> safeInterpreter c 10000 I1.interpret) $ singlecase
--    , bench "strict interpreter" $ nf (\c -> safeInterpreter c 10000 I2.interpret) $ singlecase
--    ]