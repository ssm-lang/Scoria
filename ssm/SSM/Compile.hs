-- | SSM EDSL compilation interface, for compiling to C code.
module SSM.Compile
  ( SSMProgram(..)
  , toC
  , compileFile
  , compileCli
  , compileCli_
  ) where
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )

import           SSM.Backend.C.Compile
import           SSM.Core.Syntax

-- | Compile a program to a C-file.
--
-- TODO: This can fail, so it should return Either CompileError String.
toC :: SSMProgram a => a -> String
toC = compile . toProgram

-- | Compile a program and write it to the specified path.
compileFile :: SSMProgram a => FilePath -> a -> IO ()
compileFile fp = writeFile fp . toC

-- | Create command-line compilation interface for specific program.
--
-- Includes parameter for specifying a default filepath. If this is not needed,
-- use @compileCli_@.
compileCli :: SSMProgram a => Maybe FilePath -> a -> IO ()
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
compileCli_ :: SSMProgram a => a -> IO ()
compileCli_ = compileCli Nothing
