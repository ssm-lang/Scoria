module Test.Ssm.Build
  ( doCompile
  , doMake
  , doExec
  , doVg
  ) where

import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )

import           LowCodeGen                     ( compile_ )
import           LowCore                        ( Program )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.Ssm.Report                ( (</>)
                                                , Slug(..)
                                                , slugStr
                                                , printUnixError
                                                , reportFileOnFail
                                                , reportOnFail
                                                )

-- | Tick limit compiled into the test target.
tickLimit :: Maybe Int
tickLimit = Just 7500

-- | Directory where generated C files are written to.
cSrcDir :: FilePath
cSrcDir = "genc"

-- | Name of the build platform.
buildPlatform :: String
buildPlatform = "trace"

-- | Name of the build directory.
buildDir :: FilePath
buildDir = "build" </> buildPlatform

-- | Obtain the target name from a test Slug.
--
-- Use the magic target name "arbitrary" for randomly generated tests to avoid
-- unnecessarily polluting the genc and build directories.
slugTarget :: Slug -> String
slugTarget (SlugTimestamp _) = "arbitrary"
slugTarget (SlugNamed "arbitrary") = undefined -- TODO: resolve name clash
slugTarget (SlugNamed n) = n

-- | Compile an SSM program to a C program's string representation.
doCompile :: Monad m => Slug -> Program -> QC.PropertyM m String
doCompile slug program = do
  let cSrc = compile_ True tickLimit program
  reportOnFail slug (slugTarget slug ++ ".c") cSrc
  return cSrc

-- | Try to compile a C program using make.
--
-- Note that the paths here are all relative to the root of the project path,
-- since we are already relying on stack test always executing there.
--
-- TODO: It's probably possible to just inspect the returncode here
-- TODO: pass in DEBUG flag here
doMake :: Slug -> String -> QC.PropertyM IO FilePath
doMake slug cSrc = do
  QC.run $ writeFile srcPath cSrc

  (code, out, err) <- QC.run $ readProcessWithExitCode "make" makeArgs ""
  reportFileOnFail slug execPath (slugStr slug ++ ".exe")

  case code of
    ExitSuccess   -> return execPath
    ExitFailure c -> printUnixError c out err >> fail "Make error"

 where
  target = slugTarget slug
  srcPath  = cSrcDir </> target ++ ".c"
  execPath = buildDir </> target
  makeArgs = ["PLATFORM=" ++ buildPlatform, target]

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
      | c == 123  -> printUnixError c out err >> fail "Valgrind error"
      | otherwise -> return res
  where args = ["--error-exitcode=123", "--exit-on-first-error=yes", fp]

-- | Execute a program. Succeeds even with non-zero (unsuccessful) return code.
doExec :: Slug -> FilePath -> QC.PropertyM IO (ExitCode, String, String)
doExec slug fp = do
  res@(_, out, err) <- QC.run $ readProcessWithExitCode fp [] ""
  reportOnFail slug "exec.out" out
  reportOnFail slug "exec.err" err
  return res
