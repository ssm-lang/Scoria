module Test.SSM.Prop
  ( propCompiles
  , propValgrind
  , propCorrect
  , TestName(..)
  , correctSpec
  , semanticIncorrectSpec
  ) where

import           SSM.Core.Syntax                ( Program )
import           Test.SSM.Generator             ( ) -- instance Arbitrary Program

import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.SSM.Build                 ( doCompile
                                                , doExec
                                                , doMake
                                                , doVg
                                                )
import           Test.SSM.Output                ( doCompareTraces
                                                , doInterpret
                                                , doParseOutput
                                                )
import           Test.SSM.Report                ( Slug(..)
                                                , TestName(..)
                                                , getSlug
                                                , reportProgramOnFail
                                                , reportSlug
                                                )

-- | List to store event queue sizes for testing
queueSizes :: [(Int, Int)]
queueSizes = [(32, 32), (256, 256), (2048, 2048)]

-- | Tests that generated SSM programs compile successfully.
propCompiles :: TestName -> Program -> (Int, Int) -> QC.Property
propCompiles tn program (aQSize, eQSize) = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  doMake slug cSrc (aQSize, eQSize)
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: TestName -> Program -> (Int, Int) -> QC.Property
propValgrind tn program (aQSize, eQSize) = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  fp   <- doMake slug cSrc (aQSize, eQSize)
  _    <- doVg slug fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
propCorrect :: TestName -> Program -> (Int, Int) -> QC.Property
propCorrect tn program (aQSize, eQSize) = QC.monadicIO $ do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc        <- doCompile slug program
  fp          <- doMake slug cSrc (aQSize, eQSize)
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
  once $ H.prop "compiles" $ map (propCompiles tn p) queueSizes
  once $ H.prop "runs without memory errors" $ map (propValgrind tn p) queueSizes
  once $ H.prop "runs according to interpreter" $ map (propCorrect tn p) queueSizes
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
