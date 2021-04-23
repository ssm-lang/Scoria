{-# LANGUAGE ScopedTypeVariables #-}
module Evaluation where

import System.Directory
import System.Process
import System.IO
import Control.Exception

import LowCore
import LowCodeGen
import LowInterpreter
import Trace

data Report = Good                     -- ^ Test succeeded!
            | Generated Output         -- ^ Code generator run was successful, with the output
            | Bad Output Output        -- ^ Managed to run both the interpreter and codegenerator, but they were not equal
            | CompilationError String  -- ^ Error compiling the program, with the c-file
            | ParseError String        -- ^ Error parsing output of running the program, with the output
            | ExecutionError String    -- ^ Error while running the compiled program

getname :: Program -> String
getname ssm = main ssm

{- | Given a SSM program, this function will create a new directory, compile the program in
that directory and run it, producing some output in a txt-file. This output will then be read
before the temporary directory is deleted. -}
runCG :: Program -> IO Report --(Maybe Output)
runCG program = do
    setupTestDir
    createTestFile program
    output <- runTest program
    removeTestDir
    case output of
        Left report -> return report
        Right out   -> case parseOutput $ lines out of
            Just trace -> return $ Generated trace
            Nothing    -> return $ ParseError out
  where
      -- | Name of temporary test directory
      testdir :: String
      testdir = "tmpdir/"

      -- | Location of the RTS
      rtsloc :: String
      rtsloc = "runtime/"

      -- | The command that compiles the test-file
      gcc :: String -> (String, [String])
      gcc execname = ("gcc", ["-o",execname,execname ++ ".c"] ++ flags)
        where
            flags :: [String]
            flags = [ rtssrc ++ "peng-scheduler.c"
                    , rtssrc ++ "peng-int.c"
                    , rtssrc ++ "peng-bool.c"
                    , "-I" ++ rtsloc ++ "include"
                    , "-I" ++ rtsloc ++ "linux/include"
                    , "-DDEBUG"]

            rtssrc :: String
            rtssrc = rtsloc ++ "src/"

      -- | Command that copies the RTS to the temporary test directory
      cprts :: String
      cprts = concat ["cp -r ", rtsloc, " ", testdir]

      -- | Set up the temporary test directory by creating the directory and copying the RTS there
      setupTestDir :: IO ()
      setupTestDir = do
          createDirectory testdir
          callProcess "cp" ["-r",rtsloc,testdir]
          return ()

      -- | Remove the temporary test directory
      removeTestDir :: IO ()
      removeTestDir = do
          callProcess "rm" ["-r",testdir]
          return ()

      -- | Utility function that is given a directory in which it should execute the IO action
      inDirectory :: FilePath -> IO a -> IO a
      inDirectory fp ma = do
          current <- getCurrentDirectory
          setCurrentDirectory fp
          a <- ma
          setCurrentDirectory current
          return a

      -- | Compile the test program and write it to a c-file
      createTestFile :: Program -> IO ()
      createTestFile program = do
          let name = getname program
          let c = compile_ True Nothing program

          inDirectory testdir $ do
              writeFile (name ++ ".c") c

      -- | Compile and run a test
      runTest :: Program -> IO (Either Report String)
      runTest program = do
          let name = getname program

          inDirectory testdir $ do
              let (cmd,args) = gcc name
              --callProcess cmd args
              (_,_,Just gccerr,_) <- createProcess (proc cmd args) {std_err = CreatePipe}
              c <- hGetContents gccerr
              if null c
                  then do (_, Just hout, Just herr, _) <- createProcess
                                        (proc "timeout" [show timeout ++ "s", "./" ++ name])
                                        { std_out = CreatePipe
                                        , std_err = CreatePipe
                                        }
                          err <- hGetContents herr
                          if null err
                              then Right <$> hGetContents hout
                              else return $ Left $ ExecutionError err
                  else return $ Left $ CompilationError c
        where
            timeout :: Double
            timeout = 0.2
    
      -- | Parse the output, but discard the last line. The call to timeout might have cut it
      -- off short so that it would not be parsed properly.
      parseOutput :: [String] -> Maybe Output
      parseOutput []     = Just []
      parseOutput [_]    = Just []
      parseOutput (x:xs) = do
          line <- parseLine x
          rest <- parseOutput xs
          return $ line : rest

runInterpreter :: Program -> Output
runInterpreter = interpret