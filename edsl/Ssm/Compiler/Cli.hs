module Ssm.Compiler.Cli where

import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )

import           Core                           ( SSM(..) )
import           LowCodeGen                     ( compile_ )
import           LowCore                        ( Program(..)
                                                , transpile
                                                )

compileCli :: SSM () -> IO ()
compileCli p = do
  args <- getArgs
  fp   <- getFilePath args
  compileFile p fp

 where
  getFilePath []                 = return Nothing
  getFilePath ("-h"      : _   ) = usage
  getFilePath ("-o" : fp : args) = do
    mfp <- getFilePath args
    return $ Just $ fromMaybe fp mfp
  getFilePath args = do
    putStrLn $ "Error: unrecognized arguments: " ++ unwords args
    usage

  usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [-o <out-file>]"
    exitWith $ ExitFailure 1

compileFile :: SSM () -> Maybe FilePath -> IO ()
compileFile p mfp = writeFile fp $ compile_ False Nothing p'
  where p' = transpile p
        fp = fromMaybe (entry p' ++ ".c") mfp

