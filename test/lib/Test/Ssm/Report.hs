module Test.Ssm.Report
  ( (</>)
  , reportOnFail
  , reportFileOnFail
  , reportUnixError
  , reportProgramOnFail
  , Slug(..)
  , slugStr
  , getSlug
  , TestName(..)
  , reportSlug
  ) where

import qualified Data.ByteString               as B
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           System.Directory               ( createDirectoryIfMissing
                                                , getPermissions
                                                , setPermissions
                                                )

import           SSM.Core.Syntax                ( Program )
import           SSM.Pretty                     ( prettyProgram )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

-- | Directory where reports are dumped.
reportDir :: Slug -> FilePath
reportDir (SlugTimestamp ts) = "trace-report" </> "test-" ++ show ts
reportDir (SlugNamed     n ) = "trace-report" </> "test-" ++ n

-- | Character limit for reports.
reportLimit :: Int
reportLimit = 120 * 200

-- | Timestamps are just unique integers (provided that history doesn't actually
-- repeat itself).
type Timestamp = Int

-- | Used to specify the name of a test case.
--
-- Specifying RandomTest entails that a random timestamp name will be generated
-- based on the time of the test, which is useful for generated test cases with
-- no user-defined name.
data TestName = RandomTest | NamedTest String deriving (Show, Eq)

-- | Identifier for each test run, used to determine report directory name,
-- among other things.
data Slug = SlugTimestamp Timestamp | SlugNamed String deriving (Show, Eq)

-- | Generate Slug from Testname, using the current POSIX time for timestamps.
getSlug :: TestName -> IO Slug
getSlug RandomTest       = SlugTimestamp . round . (* 1000) <$> getPOSIXTime
getSlug (NamedTest name) = return $ SlugNamed name

-- | "Decode" a Slug into its unique String.
slugStr :: Slug -> [Char]
slugStr (SlugTimestamp ts) = "Test" ++ show ts
slugStr (SlugNamed     n ) = n

-- | Report the test directory after a test fails.
reportSlug :: Monad m => Slug -> QC.PropertyM m ()
reportSlug slug = QC.monitor $ QC.counterexample $ unlines
  ["", "Report directory: " ++ reportDir slug, ""]

-- | Write string s to file at fp if the test fails
reportOnFail :: Monad m => Slug -> FilePath -> String -> QC.PropertyM m ()
reportOnFail slug fp s = QC.monitor $ QC.whenFail $ do
  createDirectoryIfMissing True $ reportDir slug
  writeFile (reportDir slug </> fp) s

-- | Copy contents of src file (as it exists now) to dst if the test fails
reportFileOnFail :: Slug -> FilePath -> FilePath -> QC.PropertyM IO ()
reportFileOnFail slug src dst = do
  -- We must read this strictly because subsequent shrinks may overwrite it
  exec <- QC.run $ B.readFile src
  perm <- QC.run $ getPermissions src

  QC.monitor $ QC.whenFail $ do
    createDirectoryIfMissing True $ reportDir slug
    B.writeFile (reportDir slug </> dst) exec
    setPermissions (reportDir slug </> dst) perm

-- | Leave both pretty-printed and regression-testable stub of program in report
-- directory if the test fails.
reportProgramOnFail :: Monad m => Slug -> Program -> QC.PropertyM m ()
reportProgramOnFail slug program = do
  reportOnFail slug (slugStr slug ++ ".ssm") $ prettyProgram program
  reportOnFail slug (slugStr slug ++ "Spec.hs") regressionSpec
 where
  -- | Format a spec that can be added to the low-regression test suite
  regressionSpec = unlines
    [ "module Regression." ++ slugStr slug ++ "Spec where"
    , ""
    , "import Data.Map (fromList)"
    , "import SSM.Core.Syntax"
    , "import qualified Test.Ssm.Prop as T"
    , "import qualified Test.Hspec as H"
    , "import qualified Test.Hspec.QuickCheck as H"
    , ""
    , "p :: Program"
    , "p = " ++ show program
    , ""
    , "spec :: H.Spec"
    , "spec = T.doProgramSpec \"" ++ slugStr slug ++ "\" p"
    ]

-- | Report a Unix error in a Quickcheck Property monad transformer.
reportUnixError
  :: Monad m => Slug -> [String] -> (Int, String, String) -> QC.PropertyM m ()
reportUnixError slug cmd (c, out, err) = do
  reportOnFail slug "unix.err" msg
  QC.monitor $ QC.counterexample msg
 where
  msg =
    "Command: "
      ++ unwords (map quote cmd)
      ++ "\n\n"
      ++ "Error code: "
      ++ show c
      ++ "\n\n"
      ++ "stdout:\n\n"
      ++ trunc "" reportLimit out
      ++ "\n----\n"
      ++ "stderr:\n\n"
      ++ trunc "" reportLimit err
      ++ "\n----\n"
  quote s = if ' ' `elem` s then ['\''] ++ s ++ ['\''] else s
  trunc acc _ []       = reverse acc
  trunc acc 0 (x : _ ) = reverse ("..." ++ x : acc)
  trunc acc n (x : xs) = trunc (x : acc) (n - 1) xs


-- | Cheap shorthand to concat a directory to a path with '/'.
--
-- In base, (++) is given its presedence with infixr 5 ++, so infixr 4 </>
-- seems appropriate.
infixr 4 </>
(</>) :: FilePath -> FilePath -> FilePath
dir </> f = dir ++ "/" ++ f
