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
