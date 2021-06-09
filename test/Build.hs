module Build
  ( doCompile
  , doMake
  , doExec
  , doVg
  ) where

import           LowCodeGen                     ( compile_ )
import           LowCore                        ( Program )
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Report                         ( (</>)
                                                , printUnixError
                                                , reportFileOnFail
                                                , reportOnFail
                                                )

-- | Tick limit compiled into the test target
testLimit :: Maybe Int
testLimit = Just 7500

-- | Name of the build directory
buildDir :: FilePath
buildDir = "build/test"

-- | Directory where module
cSrcDir :: FilePath
cSrcDir = "genc"

-- | Name of test target
targetName :: String
targetName = "quickcheckgen"

-- | Helper function that adds an extension to the target name
targetExt :: String -> FilePath
targetExt ext = targetName ++ ext

-- |
doCompile :: Monad m => Program -> QC.PropertyM m String
doCompile program = do
  let cSrc = compile_ True testLimit program
  reportOnFail (targetExt ".c") cSrc
  return cSrc

-- | Try to compile a C program using make.
--
-- Note that the paths here are all relative to the root of the project path,
-- since we are already relying on stack test always executing there.
--
-- TODO: It's probably possible to just inspect the returncode here
-- TODO: pass in DEBUG flag here
doMake :: String -> QC.PropertyM IO FilePath
doMake cSrc = do
  QC.run $ writeFile srcPath cSrc

  (code, out, err) <- QC.run $ readProcessWithExitCode "make" makeArgs ""
  reportFileOnFail execPath (targetExt ".exe")

  case code of
    ExitSuccess   -> return execPath
    ExitFailure c -> printUnixError c out err >> fail "Make error"

 where
  srcPath  = cSrcDir </> targetExt ".c"
  execPath = buildDir </> targetName
  makeArgs = ["PLATFORM=test", targetName]

-- | Test compiled program with valgrind.
--
-- We check for memory leaks by instructing valgrind to exit immediately upon
-- encountering a memory error, and with magic exit code 123.
--
-- TODO: check for memleaks too, with errors-for-leak-kinds
doVg :: FilePath -> QC.PropertyM IO (ExitCode, String, String)
doVg fp = do
  res@(code, out, err) <- QC.run $ readProcessWithExitCode "valgrind" args ""
  reportOnFail "valgrind.out" out
  reportOnFail "valgrind.err" err

  case code of
    ExitSuccess -> return res
    ExitFailure c
      | c == 123  -> printUnixError c out err >> fail "Valgrind error"
      | otherwise -> return res

  where args = ["--error-exitcode=123", "--exit-on-first-error=yes", fp]

-- | Execute a program. Succeeds even with non-zero (unsuccessful) return code.
doExec :: FilePath -> QC.PropertyM IO (ExitCode, String, String)
doExec fp = do
  res@(_, out, err) <- QC.run $ readProcessWithExitCode fp [] ""
  reportOnFail "exec.out" out
  reportOnFail "exec.err" err
  return res
