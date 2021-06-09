module Report
  ( (</>)
  , reportOnFail
  , reportFileOnFail
  , printUnixError
  , reportProgramOnFail
  ) where

import           LowCore                        ( Program )
import           LowPretty                      ( prettyProgram )

import qualified Data.ByteString               as B
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , setPermissions
                                                )
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

-- | Cheap shorthand to concat a directory to a path with /
--
-- In base (++) is given its presedence with infixr 5 ++, so infixr 4 </>
-- seems appropriate.
infixr 4 </>
(</>) :: FilePath -> FilePath -> FilePath
dir </> f = dir ++ "/" ++ f

-- | Character limit for how long reports should be
reportLimit :: Int
reportLimit = 120 * 200

-- | Directory where reports are dumped
reportDir :: FilePath
reportDir = "qc-report"

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
reportOnFail :: Monad m => FilePath -> String -> QC.PropertyM m ()
reportOnFail fp s = QC.monitor $ QC.whenFail $ do
  createDirectoryIfMissing True reportDir
  writeFile (reportDir </> fp) s

-- | Copy contents of src file (as it exists now) to dst if the test fails
reportFileOnFail :: FilePath -> FilePath -> QC.PropertyM IO ()
reportFileOnFail src dst = do
  -- We must read this strictly because subsequent shrinks may overwrite it
  exec <- QC.run $ B.readFile src
  perm <- QC.run $ getPermissions src

  QC.monitor $ QC.whenFail $ do
    createDirectoryIfMissing True reportDir
    B.writeFile (reportDir </> dst) exec
    setPermissions (reportDir </> dst) perm

reportProgramOnFail :: Monad m => FilePath -> Program -> QC.PropertyM m ()
reportProgramOnFail fp program = reportOnFail fp $ prettyProgram program
