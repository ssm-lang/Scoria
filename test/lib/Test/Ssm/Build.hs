module Test.Ssm.Build
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

import           LowCodeGen                     ( compile_ )
import           LowCore                        ( Program )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.Ssm.Report                ( (</>)
                                                , Slug(..)
                                                , reportFileOnFail
                                                , reportOnFail
                                                , reportUnixError
                                                , slugStr
                                                )

-- | Tick limit compiled into the test target.
tickLimit :: Maybe Int
tickLimit = Just 7500

-- | Name of the build platform.
buildPlatform :: String
buildPlatform = "trace"

-- | Obtain the target name from a test Slug.
--
-- Use the magic target name "arbitrary" for randomly generated tests to avoid
-- unnecessarily polluting the build directory.
slugTarget :: Slug -> String
slugTarget (SlugTimestamp _          ) = "arbitrary"
slugTarget (SlugNamed     "arbitrary") = undefined -- TODO: resolve name clash
slugTarget (SlugNamed     n          ) = n

-- | Compile an SSM program to a C program's string representation.
doCompile :: Monad m => Slug -> Program -> QC.PropertyM m String
doCompile slug program = do
  let cSrc = compile_ True tickLimit program
  reportOnFail slug (slugStr slug ++ ".c") cSrc
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
  (code, out, err) <- QC.run $ make "make_builddir"
  case code of
    ExitSuccess   -> return ()
    ExitFailure c -> do
      reportUnixError slug ("make" : mkArgs "make_builddir") (c, out, err)
      fail "Make make_builddir error"

  let execPath = dropWhileEnd isSpace out </> target

  QC.run $ writeFile (execPath ++ ".c") cSrc

  (code, out, err) <- QC.run $ make target
  case code of
    ExitSuccess -> do
      reportFileOnFail slug execPath (slugStr slug ++ ".exe")
      return execPath
    ExitFailure c -> do
      reportUnixError slug ("make" : mkArgs target) (c, out, err)
      fail "Make target error"

 where
  target = slugTarget slug
  make t = readProcessWithExitCode "make" (mkArgs t) ""
  mkArgs t = ["PLATFORM=" ++ buildPlatform, t]

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
  return res
