module Test.Ssm.Prop
  ( propCompiles
  , propValgrind
  , propCorrect
  , TestName(..)
  , correctSpec
  , semanticIncorrectSpec
  ) where

import           LowCore                        ( Program )
import           LowGenerator                   ( ) -- instance Arbitrary Program

import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
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
                                                , TestName(..)
                                                , getSlug
                                                , reportProgramOnFail
                                                , reportSlug
                                                )

-- | Tests that generated SSM programs compile successfully.
propCompiles :: TestName -> Program -> QC.Property
propCompiles tn program = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  doMake slug cSrc
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: TestName -> Program -> QC.Property
propValgrind tn program = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  fp   <- doMake slug cSrc
  _    <- doVg slug fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
propCorrect :: TestName -> Program -> QC.Property
propCorrect tn program = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc        <- doCompile slug program
  fp          <- doMake slug cSrc
  _           <- doVg slug fp
  (_, out, _) <- doExec slug fp
  cTrace      <- doParseOutput slug out
  iTrace      <- doInterpret slug program (length $ lines out)
  doCompareTraces slug iTrace cTrace

-- | Spec that ensures an SSM program compiles to valid C that compiles and runs
-- without memory errors, and behaves the same as the interpreter.
--
-- Used to build passing integration tests.
correctSpec :: String -> Program -> H.Spec
correctSpec name p = do
  once $ H.prop "compiles" $ propCompiles tn p
  once $ H.prop "runs without memory errors" $ propValgrind tn p
  once $ H.prop "runs according to interpreter" $ propCorrect tn p
 where
  once = H.modifyMaxSuccess (const 1)
  tn   = NamedTest name

-- | Spec that ensures an SSM program compiles to valid C that compiles and runs
-- without memory errors, but behaves different from the interpreter.
--
-- Used to note discrepancies with the interpreter in the regression test suite.
semanticIncorrectSpec :: String -> Program -> H.Spec
semanticIncorrectSpec name p = do
  once $ H.prop "compiles" $ propCompiles tn p
  once $ H.prop "runs without memory errors" $ propValgrind tn p
  once $ H.prop "does not match interpreter" $ QC.expectFailure $ propCorrect tn p
 where
  once = H.modifyMaxSuccess (const 1)
  tn   = NamedTest name
