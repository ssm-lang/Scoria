-- | SSM EDSL compilation interface, for compiling to C code.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module SSM.Compile
  ( toC
  , toC'
  , toC2
  , compileFile
  , compileCli
  , compileCli_
  , toProgram
  ) where
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )

import           SSM.Backend.C.Compile
import           SSM.Backend.C.Peripherals -- import this for the instances
import           SSM.Core.Program
import           SSM.Core.Backend
import           SSM.Frontend.Compile

import qualified SSM.Backend.C2.CodeGen as C2
import qualified SSM.Backend.C2.IR      as IR

toC2 :: Compile C2 () -> String
toC2 p = C2.compile $ IR.transpile $ toProgram p

-- | Compile a program to a C-file.
--
-- TODO: This can fail, so it should return Either CompileError String.
toC :: Compile C () -> String
toC p = compile $ toProgram p

toC' :: Program C -> String
toC' = compile

-- | Compile a program and write it to the specified path.
compileFile :: FilePath -> Compile C () -> IO ()
compileFile fp = writeFile fp . toC

-- | Create command-line compilation interface for specific program.
--
-- Includes parameter for specifying a default filepath. If this is not needed,
-- use @compileCli_@.
compileCli :: Maybe FilePath -> Compile C () -> IO ()
compileCli defaultPath program = do
  args <- getArgs
  path <- getFilePath args
  case (path, defaultPath) of
    (Just fp, _      ) -> compileFile fp program
    (Nothing, Just fp) -> compileFile fp program
    _                  -> do
      putStrLn "Error: No output filename specified."
      usageAndExit

 where
  getFilePath []                 = return Nothing
  getFilePath ("-h"      : _   ) = usageAndExit
  getFilePath ("-o" : fp : args) = do
    mfp <- getFilePath args
    return $ Just $ fromMaybe fp mfp
  getFilePath args = do
    putStrLn $ "Error: unrecognized arguments: " ++ unwords args
    usageAndExit

  usageAndExit = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [-o <out-file.c>]"
    exitWith $ ExitFailure 1

-- | Create command-line compilation interface for specific program.
compileCli_ :: Compile C () -> IO ()
compileCli_ = compileCli Nothing
