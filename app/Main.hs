{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List

import Frontend
import Core ()
import LowCore hiding (main)
import qualified Data.Map as Map
import qualified LowCore as LC
import qualified LowInterpreter as I1
import Data.Time ( diffUTCTime, getCurrentTime )
import System.IO
import Criterion.Main

--singlecase :: Program
--singlecase = Program {LC.main = "fun2", args = [Right ("ref3",Ref TUInt64)], funs = Map.fromList [("fun2",Procedure {name = "fun2", arguments = [("ref3",Ref TUInt64)], body = [After (Lit TUInt64 (LUInt64 2657)) ("ref3",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 28320)) (Lit TUInt64 (LUInt64 7172)) OPlus) (BOp TUInt64 (Lit TUInt64 (LUInt64 45518)) (Lit TUInt64 (LUInt64 762)) OTimes) OTimes),Wait [("ref3",Ref TUInt64)],Fork [("fun2",[Right ("ref3",Ref TUInt64)]),("fun2",[Right ("ref3",Ref TUInt64)]),("fun2",[Right ("ref3",Ref TUInt64)])],After (Lit TUInt64 (LUInt64 3429)) ("ref3",Ref TUInt64) (BOp TUInt64 (BOp TUInt64 (Lit TUInt64 (LUInt64 3880)) (Lit TUInt64 (LUInt64 36812)) OMinus) (BOp TUInt64 (Lit TUInt64 (LUInt64 10144)) (Lit TUInt64 (LUInt64 16298)) OPlus) OPlus),Wait [("ref3",Ref TUInt64)],Fork [("fun2",[Right ("ref3",Ref TUInt64)])]]})]}

--newcase :: Program
--newcase = Program {LC.main = "fun1", args = [Right ("ref3",Ref TBool),Right ("ref5",Ref TUInt64),Right ("ref6",Ref TBool),Right ("ref7",Ref TInt64)], funs = Map.fromList [("fun1",Procedure {name = "fun1", arguments = [("ref3",Ref TBool),("ref5",Ref TUInt64),("ref6",Ref TBool),("ref7",Ref TInt64)], body = [NewRef (Fresh "v0") (Ref TInt32) (BOp TInt32 (BOp TInt32 (Lit TInt32 (LInt32 41)) (Lit TInt32 (LInt32 43)) OMinus) (BOp TInt32 (Lit TInt32 (LInt32 152)) (Lit TInt32 (LInt32 143)) OPlus) OTimes),Changed (Fresh "v1") TBool ("ref7",Ref TInt64),GetRef (Fresh "v2") TBool ("ref6",Ref TBool),Changed (Fresh "v3") TBool ("v0",Ref TInt32),GetRef (Fresh "v4") TInt32 ("v0",Ref TInt32),SetRef ("ref7",Ref TInt64) (Lit TInt64 (LInt64 (-40386))),SetRef ("ref6",Ref TBool) (BOp TBool (Lit TInt32 (LInt32 189)) (Lit TInt32 (LInt32 94)) OLT),SetRef ("v0",Ref TInt32) (BOp TInt32 (Lit TInt32 (LInt32 151)) (Lit TInt32 (LInt32 36)) OTimes),SetLocal (Fresh "v1") TBool (BOp TBool (UOpE TInt32 (Var TInt32 "v4") Neg) (UOpE TInt32 (Lit TInt32 (LInt32 58)) Neg) OLT),If (BOp TBool (Lit TInt32 (LInt32 146)) (Var TInt32 "v4") OLT) [SetRef ("ref3",Ref TBool) (Lit TBool (LBool True)),SetRef ("v0",Ref TInt32) (BOp TInt32 (BOp TInt32 (Var TInt32 "v4") (Var TInt32 "v4") OMinus) (BOp TInt32 (Lit TInt32 (LInt32 15)) (Var TInt32 "v4") OTimes) OPlus),SetRef ("ref6",Ref TBool) (BOp TBool (Var TInt32 "v4") (Var TInt32 "v4") OLT),GetRef (Fresh "v7") TInt64 ("ref7",Ref TInt64)] [After (Lit TUInt64 (LUInt64 3874)) ("ref7",Ref TInt64) (Lit TInt64 (LInt64 (-50232))),NewRef (Fresh "v8") (Ref TBool) (Var TBool "v1"),GetRef (Fresh "v9") TBool ("v8",Ref TBool),Wait [("v8",Ref TBool),("ref3",Ref TBool),("ref5",Ref TUInt64),("ref6",Ref TBool),("ref7",Ref TInt64)],SetLocal (Fresh "v2") TBool (BOp TBool (Lit TInt32 (LInt32 204)) (Var TInt32 "v4") OEQ),NewRef (Fresh "v10") (Ref TInt32) (BOp TInt32 (BOp TInt32 (Var TInt32 "v4") (Var TInt32 "v4") OPlus) (BOp TInt32 (Lit TInt32 (LInt32 93)) (Lit TInt32 (LInt32 62)) OPlus) OMinus),If (Lit TBool (LBool True)) [] []],GetRef (Fresh "v11") TBool ("ref3",Ref TBool),SetRef ("ref7",Ref TInt64) (BOp TInt64 (BOp TInt64 (Lit TInt64 (LInt64 1)) (Lit TInt64 (LInt64 1)) OPlus) (BOp TInt64 (Lit TInt64 (LInt64 1)) (Lit TInt64 (LInt64 1)) OMinus) OMinus),NewRef (Fresh "v12") (Ref TInt32) (Lit TInt32 (LInt32 160)),After (Lit TUInt64 (LUInt64 2664)) ("ref5",Ref TUInt64) (Lit TUInt64 (LUInt64 13242)),NewRef (Fresh "v13") (Ref TInt32) (BOp TInt32 (BOp TInt32 (Lit TInt32 (LInt32 56)) (Var TInt32 "v4") OMinus) (BOp TInt32 (Var TInt32 "v4") (Lit TInt32 (LInt32 79)) OTimes) OMinus),Fork [("fun1",[Right ("ref6",Ref TBool),Right ("ref5",Ref TUInt64),Right ("ref3",Ref TBool),Right ("ref7",Ref TInt64)]),("fun1",[Right ("ref6",Ref TBool),Right ("ref5",Ref TUInt64),Right ("ref3",Ref TBool),Right ("ref7",Ref TInt64)])],SetRef ("v12",Ref TInt32) (Lit TInt32 (LInt32 80)),GetRef (Fresh "v14") TInt32 ("v0",Ref TInt32),GetRef (Fresh "v15") TBool ("ref3",Ref TBool),Wait [("v12",Ref TInt32),("ref3",Ref TBool),("ref5",Ref TUInt64)],NewRef (Fresh "v16") (Ref TBool) (BOp TBool (BOp TInt32 (Var TInt32 "v4") (Var TInt32 "v4") OMinus) (BOp TInt32 (Var TInt32 "v14") (Lit TInt32 (LInt32 0)) OPlus) OLT),After (Lit TUInt64 (LUInt64 3967)) ("ref7",Ref TInt64) (BOp TInt64 (Lit TInt64 (LInt64 1)) (Lit TInt64 (LInt64 1)) OPlus),SetRef ("ref3",Ref TBool) (BOp TBool (BOp TInt32 (Var TInt32 "v4") (Lit TInt32 (LInt32 76)) OPlus) (BOp TInt32 (Lit TInt32 (LInt32 163)) (Lit TInt32 (LInt32 37)) OPlus) OLT)]})]}

main :: IO ()
main = undefined --do
--  let tr = take 7500 $ I1.interpret newcase
--  if tr == (tr ++ []) then return () else return ()
  --putStrLn $ unlines $ map show $ tr
  
--  defaultMain
--    [ bench "old interpreter" $ nf (\c -> safeInterpreter c 10000 I1.interpret) $ singlecase
--    , bench "strict interpreter" $ nf (\c -> safeInterpreter c 10000 I2.interpret) $ singlecase
--    ]
