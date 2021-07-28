{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
module SSM.TestProgram where

-- NOTE: this file is only here to illustrate what a program using globals
-- can look like. It will be removed before merge.

import SSM.Interpret -- so I can load this in the repl and try it out
import SSM.Compile
import SSM.Pretty
import SSM.Language

import SSM.Core.Syntax

import Data.Word
import qualified Data.Map as Map

-- | Writes a value to the global variable
writeGlobal :: (?x :: Ref Word8) => Exp Word8 -> SSM ()
writeGlobal = box "writeGlobal" ["y"] $ \y -> do
    ?x <~ y

-- | Even though this function doesn't use ?x, it calls one that does, so it needs to
-- have the ?x constraint
progLoop :: (?x :: Ref Word8) => Exp Word8 -> SSM ()
progLoop = box "progLoop" ["y"] $ \y -> do
  fork [ writeGlobal y ]
  fork [ progLoop (y + 1) ]

-- | top level program
program :: Compile (SSM ())
program = do
    -- create global reference
    globalref <- global @Word8


    -- give it whatever name we chose in our constraint
    let ?x = globalref

    -- the program
    return $ progLoop 0

p = Program {entry = "fun2", args = [Left (BOp TInt64 (Lit TInt64 (LInt64 (-9836))) (Lit TInt64 (LInt64 23310)) OMinus),Left (BOp TBool (Lit TBool (LBool False)) (Lit TBool (LBool True)) OEQ),Right (Dynamic ("ref3",Ref TBool)),Right (Dynamic ("ref4",Ref TInt32)),Right (Dynamic ("ref5",Ref TInt32)),Right (Dynamic ("ref6",Ref TUInt64)),Left (BOp TInt64 (Lit TInt64 (LInt64 46512)) (Lit TInt64 (LInt64 (-2612))) OMinus),Right (Dynamic ("ref8",Ref TBool)),Right (Dynamic ("ref9",Ref TBool)),Left (BOp TInt32 (Lit TInt32 (LInt32 203)) (Lit TInt32 (LInt32 35)) OMinus)], funs = Map.fromList [("fun2",Procedure {name = "fun2", arguments = [("var1",TInt64),("var2",TBool),("ref3",Ref TBool),("ref4",Ref TInt32),("ref5",Ref TInt32),("ref6",Ref TUInt64),("var7",TInt64),("ref8",Ref TBool),("ref9",Ref TBool),("var10",TInt32)], body = [GetRef (Fresh "v0") TInt64 (Static ("glob3",Ref TInt64)),Fork [("fun3",[Left (UOpR TBool (Static ("glob3",Ref TInt64)) Changed)])],Skip,Skip,Skip,Skip,Skip,NewRef (Fresh "v15") TBool (UOpR TBool (Dynamic ("ref3",Ref TBool)) Changed),Skip,Skip,Skip,Skip,Skip,Skip,NewRef (Fresh "v18") TBool (BOp TBool (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OPlus) (BOp TInt32 (Lit TInt32 (LInt32 1)) (Lit TInt32 (LInt32 1)) OPlus) OEQ)]}),("fun3",Procedure {name = "fun3", arguments = [("var1",TBool)], body = [Skip,Skip,Skip,Skip,Skip,Skip]})], global_references = [("glob0",Ref TInt64),("glob1",Ref TUInt64),("glob2",Ref TUInt64),("glob3",Ref TInt64)]}
