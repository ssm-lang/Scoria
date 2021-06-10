module Report
  ( (</>)
  , reportOnFail
  , reportFileOnFail
  , printUnixError
  , reportProgramOnFail
  , Slug(..)
  , getTimestamp
  , reportSlug
  ) where

import           LowCore                        ( Program )
import           LowPretty                      ( prettyProgram )

import qualified Data.ByteString               as B
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , setPermissions
                                                )
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

-- | Identifier for each test, used to determine report directory name.
type Slug = String

-- | Generate a test Slug using the current POSIX time.
--
-- Useful for generated test cases with no user-defined name.
getTimestamp :: IO Slug
getTimestamp = show . round . (* 1000) <$> getPOSIXTime

-- | Cheap shorthand to concat a directory to a path with /
--
-- In base (++) is given its presedence with infixr 5 ++, so infixr 4 </>
-- seems appropriate.
infixr 4 </>
(</>) :: FilePath -> FilePath -> FilePath
dir </> f = dir ++ "/" ++ f

-- | Directory where reports are dumped
reportDir :: Slug -> FilePath
reportDir sl = "trace-report" </> "test-" ++ sl

-- | Character limit for reports
reportLimit :: Int
reportLimit = 120 * 200

-- | Report the test directory after a test fails
reportSlug :: Monad m => Slug -> QC.PropertyM m ()
reportSlug sl =
  QC.monitor $ QC.counterexample $ "Report directory: " ++ reportDir sl

-- | Print a Unix error in a Quickcheck Property monad transformer
printUnixError :: Monad m => Int -> String -> String -> QC.PropertyM m ()
printUnixError c out err =
  QC.monitor
    $  QC.counterexample
    $  "Error code: "
    ++ show c
    ++ "\n\n"
    ++ "stdout:\n"
    ++ out'
    ++ "\n\n"
    ++ "stderr:\n"
    ++ err'
    ++ "\n"
 where
  out' = trunc "" reportLimit out
  err' = trunc "" reportLimit err
  trunc acc _ []       = reverse acc
  trunc acc 0 (x : _ ) = reverse ("..." ++ x : acc)
  trunc acc n (x : xs) = trunc (x : acc) (n - 1) xs

-- | Write string s to file at fp if the test fails
reportOnFail :: Monad m => Slug -> FilePath -> String -> QC.PropertyM m ()
reportOnFail sl fp s = QC.monitor $ QC.whenFail $ do
  createDirectoryIfMissing True $ reportDir sl
  writeFile (reportDir sl </> fp) s

-- | Copy contents of src file (as it exists now) to dst if the test fails
reportFileOnFail :: Slug -> FilePath -> FilePath -> QC.PropertyM IO ()
reportFileOnFail sl src dst = do
  -- We must read this strictly because subsequent shrinks may overwrite it
  exec <- QC.run $ B.readFile src
  perm <- QC.run $ getPermissions src

  QC.monitor $ QC.whenFail $ do
    createDirectoryIfMissing True $ reportDir sl
    B.writeFile (reportDir sl </> dst) exec
    setPermissions (reportDir sl </> dst) perm

reportProgramOnFail
  :: Monad m => Slug -> FilePath -> Program -> QC.PropertyM m ()
reportProgramOnFail sl fp program = reportOnFail sl fp $ prettyProgram program
