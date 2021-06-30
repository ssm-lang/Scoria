module Test.SSM.Prop
  ( propCompiles
  , propValgrind
  , propCorrect
  , TestName(..)
  , doProgramSpec
  ) where

import           SSM.Core.Syntax                ( Program )
import           Test.SSM.Generator             ( ) -- instance Arbitrary Program

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.Hspec                     ( Spec(..) )
import           Test.Hspec.QuickCheck          ( modifyMaxSuccess
                                                , prop
                                                )

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

doProgramSpec :: String -> Program -> Spec
doProgramSpec name p = do
  once $ prop "compiles" $ propCompiles tn p
  once $ prop "runs without memory errors" $ propValgrind tn p
  once $ prop "runs according to interpreter" $ propCorrect tn p
 where
  once = modifyMaxSuccess (const 1)
  tn   = NamedTest name
