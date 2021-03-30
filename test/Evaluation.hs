module Evaluation (runCG, runInterpreter) where

import System.Directory
import System.Process

import Frontend ( SSM, getname )
import CodeGen (compile)
import Interpreter (interpret)
import Trace

{- | Given a SSM program, this function will create a new directory, compile the program in
that directory and run it, producing some output in a txt-file. This output will then be read
before the temporary directory is deleted. -}
runCG :: SSM () -> IO String
runCG program = do
    setupTestDir
    createTestFile program
    runTest program
    output <- getOutput program
    removeTestDir
    return output
  where
      -- | Name of temporary test directory
      testdir :: String
      testdir = "tmpdir/"

      -- | Location of the RTS
      rtsloc :: String
      rtsloc = "runtime/"

      -- | The command that compiles the test-file
      gcc :: String -> String
      gcc execname = concat ["gcc -o ", execname, " ", execname, ".c", flags]
        where
            flags :: String
            flags = concat [ " "
                           , rtssrc, "peng-scheduler.c "
                           , rtssrc, "peng-int.c "
                           , rtssrc, "peng-bool.c -I"
                           , rtsloc, "include -I"
                           , rtsloc, "linux/include "
                           , " -DDEBUG"]

            rtssrc :: String
            rtssrc = rtsloc ++ "src/"

      -- | Command that copies the RTS to the temporary test directory
      cprts :: String
      cprts = concat ["cp -r ", rtsloc, " ", testdir]

      -- | Set up the temporary test directory by creating the directory and copying the RTS there
      setupTestDir :: IO ()
      setupTestDir = do
          createDirectory testdir
          system cprts
          return ()

      -- | Remove the temporary test directory
      removeTestDir :: IO ()
      removeTestDir = do
          system $ "rm -r " ++ testdir
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
      createTestFile :: SSM () -> IO ()
      createTestFile program = do
          let name = getname program
          let c = compile True Nothing program

          inDirectory testdir $ do
              writeFile (name ++ ".c") c

      -- | Compile and run a test
      runTest :: SSM () -> IO ()
      runTest program = do
          let name = getname program

          inDirectory testdir $ do
              system $ gcc name
              system $ concat ["./", name, " > ", name, ".txt"]
        
          return ()

      -- | Get the output of running a test. The output will have been written to a txt-file.
      getOutput :: SSM () -> IO String
      getOutput program = do
          let name = getname program

          inDirectory testdir $ do
              readFile $ name ++ ".txt"

runInterpreter :: SSM () -> Output
runInterpreter = interpret