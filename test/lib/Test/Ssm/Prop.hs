module Test.Ssm.Prop
  ( propCompiles
  , propValgrind
  , propCorrect
  , TestName(..)
  ) where

import           LowCore                        ( Program )
import           LowGenerator                   ( ) -- instance Arbitrary Program

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.Ssm.Build                 ( doCompile
                                                , doExec
                                                , doMake
                                                , doVg
                                                )
import           Test.Ssm.Output                ( doCompareTraces
                                                , doInterpret
                                                , doParseOutput
                                                )
import           Test.Ssm.Report                ( Slug(..)
                                                , getTimestamp
                                                , reportProgramOnFail
                                                , reportSlug
                                                )


data TestName = RandomTest | NamedTest String deriving (Show, Eq)

-- | Name of test target
genName :: String
genName = "quickcheckgen"

-- | Tests that generated SSM programs compile successfully.
propCompiles :: TestName -> Program -> QC.Property
propCompiles sl program = QC.monadicIO $ do
  (slug, name) <- case sl of
    RandomTest -> do
      s <- QC.run getTimestamp
      return (s, genName)
    NamedTest n -> return (n, n)
  reportSlug slug
  reportProgramOnFail slug name program
  cSrc <- doCompile slug name program
  doMake slug name cSrc
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: TestName -> Program -> QC.Property
propValgrind sl program = QC.monadicIO $ do
  (slug, name) <- case sl of
    RandomTest -> do
      s <- QC.run getTimestamp
      return (s, genName)
    NamedTest n -> return (n, n)
  reportSlug slug
  reportProgramOnFail slug name program
  cSrc <- doCompile slug name program
  fp   <- doMake slug name cSrc
  _    <- doVg slug fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
propCorrect :: TestName -> Program -> QC.Property
propCorrect sl program = QC.monadicIO $ do
  (slug, name) <- case sl of
    RandomTest -> do
      s <- QC.run getTimestamp
      return (s, genName)
    NamedTest n -> return (n, n)
  reportSlug slug
  reportProgramOnFail slug name program
  cSrc        <- doCompile slug name program
  fp          <- doMake slug name cSrc
  _           <- doVg slug fp
  (_, out, _) <- doExec slug fp
  cTrace      <- doParseOutput slug out
  iTrace      <- doInterpret slug program (length $ lines out)
  doCompareTraces slug iTrace cTrace
