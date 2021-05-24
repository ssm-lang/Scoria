{-# LANGUAGE ScopedTypeVariables #-}
module Evaluation where

import System.Directory
import System.Process
import System.Exit
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
  deriving Show

getname :: Program -> String
getname ssm = main ssm

-- | This function runs a program by creating a temporary directory, copying the
-- runtime system there, compiling the program and then running the executable.
-- It will parse the output of the program and return a report.
runCG :: Program -> Maybe Int -> IO Report
runCG program mi = do
--    putStrLn "creating test dir"
    setupTestDir
--    putStrLn "created test dir"
    createTestFile program True mi
--    putStrLn "created test file"
--    putStrLn "starting execution of program"
    output <- runTest program
--    putStrLn "finished executing program"
    removeTestDir
--    putStrLn "removed test dir"
    case output of
        Left report -> return report
        Right out   -> case parseOutput $ lines out of
            Just trace -> return $ Generated trace
            Nothing    -> return $ ParseError out

-- | For running valgrind, we need to stay away from the linux timeout call, as it will mask
-- the memory behavior of the program we are inspection. Instead we use the second argument to
-- compile to indicate for how many seconds the program will run, and it will generate a thread
-- that terminates the thread after that time.
runCGValgrind :: Program -> Maybe Int -> IO Bool
runCGValgrind p mi = do
    setupTestDir
    createTestFile p True mi
    c <- tryCompile p True
    case c of
        Left _  -> do removeTestDir
                      return True
        Right _ -> do b <- runExecutableCheckCode (getname p) wrapValgrind
                      removeTestDir
                      return b
  where
      wrapValgrind :: Maybe (String -> (String, [String]))
      wrapValgrind = Just $ \cmd -> ("valgrind", ["--error-exitcode=123", cmd])

runCGCompilation :: Program -> IO Bool
runCGCompilation p = do
    setupTestDir
    createTestFile p True Nothing
    c <- tryCompile p True
    removeTestDir
    case c of
        Left _  -> return False
        Right _ -> return True

{-********** Utility functions for generating code and running tests **********-}

-- | Name of temporary test directory
testdir :: String
testdir = "tmpdir/"

-- | Location of the RTS
rtsloc :: String
rtsloc = "runtime/"

-- | The command that compiles the test-file
gcc :: String -> Bool -> (String, [String])
gcc execname debug = ("gcc", ["-o",execname,execname ++ ".c"] ++ flags)
  where
      flags :: [String]
      flags = [ rtssrc ++ "peng-scheduler.c"
              , rtssrc ++ "peng-int.c"
              , rtssrc ++ "peng-int64.c"
              , rtssrc ++ "peng-uint8.c"
              , rtssrc ++ "peng-bool.c"
              , "-I" ++ rtsloc ++ "include"
              , "-I" ++ rtsloc ++ "linux/include"
              , "-g"
              ] ++ if debug then ["-DDEBUG"] else []

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
createTestFile :: Program -> Bool -> Maybe Int -> IO ()
createTestFile program b d = do
    let name = getname program
    let c = compile_ b d program

    inDirectory testdir $ do
        writeFile (name ++ ".c") c

-- | Try to compile a program in the test directory. The bool signifies if the
-- debug flag should be enabled while compiling. Without the flag the executable is
-- much less verbose about what it is doing.
-- TODO: It's probably possible to just inspect the returncode here
tryCompile :: Program -> Bool -> IO (Either String ())
tryCompile p debug = inDirectory testdir $ do
    let name = getname p
    let (cmd, args) = gcc name debug
    (_,_,Just gccerr,_) <- createProcess (proc cmd args) {std_err = CreatePipe }
    c <- System.IO.hGetContents gccerr
    if null c
        then return $ Right ()
        else putStrLn c >> return (Left c)

-- | Tries to run an executable. The Maybe (String -> String) is a function that
-- takes the execute-call as a parameter and wraps it in another call, e.g timeout,
-- valgrind etc, and gives potential arguments.
runExecutable :: String -> Maybe (String -> (String,[String])) -> IO (Either String String)
runExecutable exec m = do
    let cmd'        = "./" ++ exec
    let (cmd, args) = maybe (cmd', []) (\f -> f cmd') m
    inDirectory testdir $ do
        (_,Just hout, Just herr, _) <- createProcess (proc cmd args) { std_out = CreatePipe --Inherit
                                                                     , std_err = CreatePipe --Inherit
                                                                     }
        err <- hGetContents herr
        std <- hGetContents hout
        
        -- Koen, when you see this hack, remind me to ask you about this!
        -- / Robert
        let l = length std
        if l == l then return () else return ()
        
        if null err
            then return $ Right std
            else return $ Left err

-- | Intended to be used with valgrind. Arguments are the same as the function above this
-- one, and the result is True if the error code that is returned is not equal to 1 (the
-- error code we ask valgrind to return if it finds error), and otherwise false.
runExecutableCheckCode :: String -> Maybe (String -> (String, [String])) -> IO Bool
runExecutableCheckCode exec m = do
    let cmd'        = "./" ++ exec
    let (cmd, args) = maybe (cmd, []) (\f -> f cmd') m
    inDirectory testdir $ do
        (_,_,_,p) <- createProcess (proc cmd args) { std_out = CreatePipe
                                                   , std_err = CreatePipe
                                                   }
        ex        <- waitForProcess p
        return $ not $ ex == (ExitFailure 123)

-- | Compile and run a test
runTest :: Program -> IO (Either Report String)
runTest program = do
    let name = getname program
--    putStrLn "got name of executable"

--    putStrLn "trying to compile the file"
    comp <- tryCompile program True
--    putStrLn "compiled the file"
    case comp of
        Left c  -> return $ Left $ CompilationError c
        Right _ -> do
            let f = Just (\cmd -> (cmd, []))
            res <- runExecutable name f
            case res of
                Left c  -> return $ Left $ ExecutionError c
                Right s -> return $ Right s

-- | Parse the output, but discard the last line. The call to timeout might have cut it
-- off short so that it would not be parsed properly.
parseOutput :: [String] -> Maybe Output
parseOutput []     = Just []
parseOutput (x:xs) = do
    line <- parseLine x
    rest <- parseOutput xs
    return $ line : rest

runInterpreter :: Program -> Output
runInterpreter = interpret