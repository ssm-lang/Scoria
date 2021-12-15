{-# LANGUAGE FlexibleContexts #-}
module Test.SSM.Build
  ( doCompile
  , doMake
  , doExec
  , doVg
  ) where

import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd )
import           System.Directory               ( createDirectoryIfMissing )
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )

import           SSM.Core                       ( Program, C )
import           SSM.Compile                    ( toC' )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.SSM.Report                ( (</>)
                                                , Slug(..)
                                                , reportDir
                                                , reportFileOnFail
                                                , reportOnFail
                                                , reportScriptOnFail
                                                , reportUnixError
                                                )

-- | Name of the build platform.
buildPlatform :: String
buildPlatform = "trace"

-- | Compile an SSM program to a C program's string representation.
doCompile :: Monad m => Slug -> Program C -> QC.PropertyM m String
doCompile slug program = do
  let cSrc = toC' program
  reportOnFail slug (show slug ++ ".c") cSrc
  return cSrc

-- | Compile a C program using make; returns path of the built executable.
--
-- Note that the paths here are all relative to the root of the project path,
-- since we are already relying on stack test always executing there.
doMake :: Slug -> String -> (Int, Int) -> QC.PropertyM IO FilePath
doMake slug cSrc (aQSize, eQSize) = do
  -- Remove and recreate build directory to ensure clean build.
  _   <- make "clean"
  out <- make "make_builddir"

  let -- Where the build will take place
      buildDir = dropWhileEnd isSpace out
      -- Path of executable in build directory
      execPath = buildDir </> target
      -- Path of C source file in build directory
      srcPath  = execPath ++ ".c"

  -- Add C source file to build directory, so that it can be compiled.
  QC.run $ writeFile srcPath cSrc
  -- Add helper script to make the previous step easier.
  reportScriptOnFail slug "load-c"
    $ loadCScript (reportDir slug </> target ++ ".c") buildDir srcPath

  -- Build executable.
  _ <- make target

  -- Add executable to report directory upon failure.
  reportFileOnFail slug execPath (target ++ ".exe")

  return execPath
 where
  target = show slug

  -- | Build list of parameters to pass to make, in addition to target 't'.
  mkArgs t =
    [ "PLATFORM=" ++ buildPlatform
    , "SSM_ACT_QUEUE_SIZE=" ++ show aQSize
    , "SSM_EVENT_QUEUE_SIZE=" ++ show eQSize
    , t
    ]

  -- | Call make with target, and handle error(s), if any.
  make t = do
    (code, out, err) <- QC.run $ readProcessWithExitCode "make" (mkArgs t) ""
    case code of
      ExitSuccess   -> return out
      ExitFailure c -> do
        reportUnixError slug ("make" : mkArgs t) (c, out, err)
        fail $ "Make error: " ++ t

  -- | Generate helper script that loads the generated .c file in the report
  --   directory back into the build directory.
  loadCScript src dstDir dst = unlines
    [ "#!/usr/bin/env bash"
    , ""
    , "GITROOT=\"$(git rev-parse --show-toplevel)\""
    , ""
    , "echo mkdir -p \"" ++ dstDir ++ "\""
    , "mkdir -p \"" ++ dstDir ++ "\""
    , ""
    , "echo cp \"$GITROOT/" ++ src ++ "\" \"" ++ dst ++ "\""
    , "cp \"$GITROOT/" ++ src ++ "\" \"" ++ dst ++ "\""
    , ""
    , "echo \"# Copied generated .c file into the build directory\""
    , "echo \"# To build, run:\""
    , "echo \"    make " ++ unwords (mkArgs target) ++ "\""
    ]

-- | Test compiled program with valgrind.
--
-- We check for memory leaks by instructing valgrind to exit immediately upon
-- encountering a memory error, and with magic exit code 123.
--
-- TODO: check for memleaks too, with errors-for-leak-kinds
doVg :: Slug -> FilePath -> QC.PropertyM IO (ExitCode, String, String)
doVg slug fp = do
  res@(code, out, err) <- QC.run $ readProcessWithExitCode "valgrind" args ""
  reportOnFail slug "valgrind.out" out
  reportOnFail slug "valgrind.err" err

  case code of
    ExitSuccess -> return res
    ExitFailure c
      | c == 123 -> do
        reportUnixError slug ("valgrind" : args) (c, out, err)
        fail "Valgrind error"
      | otherwise -> return res
  where args = ["--error-exitcode=123", "--exit-on-first-error=yes", fp]

-- | Execute a program. Succeeds even with non-zero (unsuccessful) return code.
doExec :: Slug -> FilePath -> QC.PropertyM IO (ExitCode, String, String)
doExec slug fp = do
  res@(_, out, err) <- QC.run $ readProcessWithExitCode fp [] ""
  reportOnFail slug "exec.out" out
  reportOnFail slug "exec.err" err
  if null err
    then return res
    else fail "Unexpected stderr output (see trace-report)"
