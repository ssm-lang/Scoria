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

import           SSM.Backend.C.Compile          ( compile )
import           SSM.Core.Syntax                ( Program )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.SSM.Report                ( (</>)
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

-- | Size of the act queue
actQueueSize :: Int
actQueueSize = 1024

-- | Size of the act queue
eventQueueSize :: Int
eventQueueSize = 2048

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
  let cSrc = compile program True tickLimit
  reportOnFail slug (slugTarget slug ++ ".c") cSrc
  return cSrc

-- | Try to compile a C program using make.
--
-- Note that the paths here are all relative to the root of the project path,
-- since we are already relying on stack test always executing there.
--
-- TODO: It's probably possible to just inspect the returncode here
-- TODO: pass in DEBUG flag here
doMake :: Slug -> String -> (Int, Int) -> QC.PropertyM IO FilePath
doMake slug cSrc (aQSize, eQSize) = do
  _   <- make "clean"
  out <- make "make_builddir"

  let execPath = dropWhileEnd isSpace out </> target
  QC.run $ writeFile (execPath ++ ".c") cSrc

  _ <- make target
  reportFileOnFail slug execPath (slugStr slug ++ ".exe")

  return execPath
 where
  target = slugTarget slug
  mkArgs t = ["PLATFORM=" ++ buildPlatform, "SSM_ACT_QUEUE_SIZE=" ++ show aQSize, "SSM_EVENT_QUEUE_SIZE=" ++ show eQSize, t]
  make t = do
    (code, out, err) <- QC.run $ readProcessWithExitCode "make" (mkArgs t) ""
    case code of
      ExitSuccess   -> return out
      ExitFailure c -> do
        reportUnixError slug ("make" : mkArgs t) (c, out, err)
        fail "Make target error"

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
  QC.assert $ null err
  reportOnFail slug "exec.out" out
  reportOnFail slug "exec.err" err
  return res
