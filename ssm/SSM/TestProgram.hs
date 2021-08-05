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
import SSM.Util.Default

import Data.Word
import qualified Data.Map as Map

p = Program {entry = Ident {identName = "fun0", identSrcInfo = Nothing}, args = [], funs = Map.fromList [(Ident {identName = "fun0", identSrcInfo = Nothing},Procedure {name = Ident {identName = "fun0", identSrcInfo = Nothing}, arguments = [], body = [GetRef (Ident {identName = "v0", identSrcInfo = Nothing}) TInt64 (Static (Ident {identName = "glob0", identSrcInfo = Nothing},Ref TInt64)),Skip,If (UOpR TBool (Static (Ident {identName = "glob2", identSrcInfo = Nothing},Ref TInt64)) Changed) [Wait [Static (Ident {identName = "glob1", identSrcInfo = Nothing},Ref TEvent)]] [NewRef (Ident {identName = "v1", identSrcInfo = Nothing}) TUInt64 (Lit TUInt64 (LUInt64 11391))],NewRef (Ident {identName = "v2", identSrcInfo = Nothing}) TInt64 (Var TInt64 (Ident {identName = "v0", identSrcInfo = Nothing})),NewRef (Ident {identName = "v3", identSrcInfo = Nothing}) TInt32 (Lit TInt32 (LInt32 2))]})], globalReferences = [(Ident {identName = "glob0", identSrcInfo = Nothing},Ref TInt64),(Ident {identName = "glob1", identSrcInfo = Nothing},Ref TEvent),(Ident {identName = "glob2", identSrcInfo = Nothing},Ref TInt64)]}


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
