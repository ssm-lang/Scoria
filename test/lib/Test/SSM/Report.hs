module Test.SSM.Report
  ( (</>)
  , reportDir
  , reportOnFail
  , reportFileOnFail
  , reportScriptOnFail
  , reportUnixError
  , reportProgramOnFail
  , Slug(..)
  , getSlug
  , TestName(..)
  , reportSlug
  ) where

import qualified Data.ByteString               as B
import           System.Directory               ( createDirectoryIfMissing
                                                , doesPathExist
                                                , getPermissions
                                                , removePathForcibly
                                                , setOwnerExecutable
                                                , setPermissions
                                                )

import           SSM.Compile                    ( SSMProgram(..) )
import           SSM.Pretty                     ( prettyProgram )

import           Data.Char                      ( isUpper )
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

-- | Directory where reports are dumped.
reportDir :: Slug -> FilePath
reportDir sl = "trace-report" </> show sl

lowRegressionSuiteDir :: FilePath
lowRegressionSuiteDir = "test/regression-low/Regression"

-- | Used to specify the name of a test case.
--
-- Specifying RandomTest entails that a random timestamp name will be generated,
-- useful for generated test cases with no user-defined name.
data TestName = RandomTest | NamedTest String deriving (Show, Eq)

-- | Identifier for each test run, used to determine report directory name.
data Slug = SlugGenerated Int | SlugNamed String deriving Eq

instance Show Slug where
  show (SlugGenerated i) = "Arb" ++ show i
  show (SlugNamed     n) = n

-- | Generate Slug from TestName, using the current POSIX time for timestamps.
--
-- In the case of named directories, delete the directory first.
--
-- Fails if the name is not valid, i.e., an empty string or not beginning with
-- an upper case letter.
getSlug :: TestName -> IO Slug
getSlug (NamedTest name)
  | validName name = removePathForcibly (reportDir sl) >> return sl
  | otherwise      = fail $ "Not a valid test name: '" ++ name ++ "'"
 where
  sl = SlugNamed name
  validName name = (not . null) name && (isUpper . head) name
getSlug RandomTest = getSlug' 1
 where
  getSlug' i = do
    let sl = SlugGenerated i
    pathExists <- doesPathExist $ reportDir sl
    if pathExists then getSlug' (i + 1) else return sl

-- | Report the test directory after a test fails.
reportSlug :: Monad m => Slug -> QC.PropertyM m ()
reportSlug slug = QC.monitor $ QC.counterexample $ unlines
  ["", "Report directory: " ++ reportDir slug, ""]

-- | Write string s to file at fp if the test fails.
reportOnFail :: Monad m => Slug -> FilePath -> String -> QC.PropertyM m ()
reportOnFail slug fp s = QC.monitor $ QC.whenFail $ do
  createDirectoryIfMissing True $ reportDir slug
  writeFile (reportDir slug </> fp) s

-- | Write string s to file with exec permissions at fp if the test fails.
reportScriptOnFail
  :: Monad m => Slug -> FilePath -> String -> QC.PropertyM m ()
reportScriptOnFail slug fp s = QC.monitor $ QC.whenFail $ do
  createDirectoryIfMissing True $ reportDir slug
  writeFile path s
  perm <- getPermissions path
  setPermissions path $ setOwnerExecutable True perm
  where path = reportDir slug </> fp

-- | Copy contents of src file (as it exists now) to dst if the test fails.
reportFileOnFail :: Slug -> FilePath -> FilePath -> QC.PropertyM IO ()
reportFileOnFail slug src dst = do
  -- Read file strictly because subsequent shrinks may overwrite it
  exec <- QC.run $ B.readFile src
  perm <- QC.run $ getPermissions src

  QC.monitor $ QC.whenFail $ do
    createDirectoryIfMissing True $ reportDir slug
    B.writeFile (reportDir slug </> dst) exec
    setPermissions (reportDir slug </> dst) perm

-- | Leave both pretty-printed and regression-testable stub of program in report
-- directory if the test fails.
reportProgramOnFail
  :: (Monad m, SSMProgram p) => Slug -> p -> QC.PropertyM m ()
reportProgramOnFail slug program = do
  reportOnFail slug (show slug ++ ".ssm") $ prettyProgram $ toProgram program
  reportOnFail slug (show slug ++ "Spec.hs") regressionSpec
  reportScriptOnFail slug "save-regression" saveSpecScript
 where
  -- | Format a spec that can be added to the low-regression test suite
  regressionSpec = unlines
    [ "-- | FIXME: add documentation about this regression test."
    , "--"
    , "-- Use template below when appropriate."
    , "--"
    , "-- Bug encountered: (what happened)"
    , "-- (Suspected) cause: (why it happened)"
    , "-- Fix: (suggestion/plan for how to fix)"
    , "-- Fixed: (include commit hash if already fixed; otherwise write 'notyet' or 'wontfix')"
    , "--"
    , "-- Include links to GitHub issues if any are created."
    , "module Regression." ++ show slug ++ "Spec where"
    , ""
    , "import Data.Map (fromList)"
    , "import SSM.Core.Syntax"
    , "import qualified Test.SSM.Prop as T"
    , "import qualified Test.Hspec as H"
    , "import qualified Test.Hspec.QuickCheck as H"
    , ""
    , "spec :: H.Spec"
    , "spec = T.correctSpec \"" ++ show slug ++ "\" p"
    , ""
    , "p :: Program"
    , "p = " ++ show (toProgram program)
    ]

  saveSpecScript = unlines
    [ "#!/usr/bin/env bash"
    , "GITROOT=\"$(git rev-parse --show-toplevel)\""
    , ""
    , "echo cp \"$GITROOT/" ++ src ++ "\" \"$GITROOT/" ++ dst ++ "\""
    , "cp \"$GITROOT/" ++ src ++ "\" \"$GITROOT/" ++ dst ++ "\""
    , ""
    , "echo \"# Copied Spec file into the regression test suite.\""
    ]
   where
    src  = reportDir slug </> spec
    dst  = lowRegressionSuiteDir </> spec
    spec = show slug ++ "Spec.hs"

-- | Report a Unix error in a Quickcheck Property monad transformer.
reportUnixError
  :: Monad m => Slug -> [String] -> (Int, String, String) -> QC.PropertyM m ()
reportUnixError slug cmd (c, out, err) = do
  reportOnFail slug filename $ msg trunc
  QC.monitor $ QC.counterexample $ msg notrunc
 where
  filename = if null cmd then "unix.err" else head cmd
  msg t = unlines
    [ "Command: "
    , unwords (map quoteCmd cmd)
    , ""
    , "Error code: "
    , show c
    , ""
    , "stdout:"
    , ""
    , t "" reportLimit out
    , "----"
    , "stderr:"
    , ""
    , t "" reportLimit err
    , "----"
    ]

  -- | Wrap command in quotation marks if there is a space in it.
  quoteCmd :: String -> String
  quoteCmd s = if ' ' `elem` s then ['\''] ++ s ++ ['\''] else s

  -- | Character limit for truncation.
  reportLimit :: Int
  reportLimit = 120 * 200

  -- | Truncate a string so as to not clutter the console.
  trunc :: String -> Int -> String -> String
  trunc acc _ []       = reverse acc
  trunc acc 0 (x : _ ) = reverse ("..." ++ x : acc)
  trunc acc n (x : xs) = trunc (x : acc) (n - 1) xs

  -- | Don't truncate; do nothing.
  notrunc :: String -> Int -> String -> String
  notrunc _ _ s = s

-- | Cheap shorthand to concat a directory to a path with '/'.
--
-- In base, (++) is given its presedence with infixr 5 ++, so infixr 4 </>
-- seems appropriate.
infixr 4 </>
(</>) :: FilePath -> FilePath -> FilePath
dir </> f = dir ++ "/" ++ f
