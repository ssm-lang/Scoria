{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Test.SSM.Prop
  ( propCompiles
  , propValgrind
  , propCorrect
  , propSyntacticEquality
  , TestName(..)
  , correctSpec
  , semanticIncorrectSpec
  ) where

import           Unsafe.Coerce                  ( unsafeCoerce )
import           SSM.Core                       ( Program
                                                , C
                                                , Interpret
                                                , PrettyPrint
                                                , Backend(..)
                                                )
import           SSM.Compile
import           SSM.Pretty
import           Test.SSM.QuickCheck.Generator  ( ) -- instance Arbitrary Program

import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.SSM.Build                 ( doCompile
                                                , doExec
                                                , doMake
                                                , doVg
                                                )
import           Test.SSM.Report                ( Slug(..)
                                                , TestName(..)
                                                , getSlug
                                                , reportProgramOnFail
                                                , reportSlug
                                                )
import           Test.SSM.Trace                 ( doCompareTraces
                                                , doInterpret
                                                , doParseOutput
                                                )

-- | List of act and event queue sizes to test.
queueSizes :: [(Int, Int)]
queueSizes = [(32, 32), (256, 256), (2048, 2048)]

-- | Tests that generated SSM programs compile successfully.
propCompiles :: TestName -> (forall backend . Backend backend => Program backend) -> QC.Property
propCompiles tn program =
  QC.monadicIO $ mapM_ (propCompilesWithSize tn program) queueSizes

-- | Tests that generated SSM programs compile successfully, given some size.
propCompilesWithSize :: TestName -> (forall backend . Backend backend => Program backend) -> (Int, Int) -> QC.PropertyM IO ()
propCompilesWithSize tn program (aQSize, eQSize) = do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  doMake slug cSrc (aQSize, eQSize)
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: TestName -> (forall backend . Backend backend => Program backend) -> QC.Property
propValgrind tn program =
  QC.monadicIO $ mapM_ (propValgrindWithSize tn program) queueSizes

-- | Tests an SSM program by evaluating it under valgrind, given some size
propValgrindWithSize :: TestName -> (forall backend . Backend backend => Program backend) -> (Int, Int) -> QC.PropertyM IO ()
propValgrindWithSize tn program (aQSize, eQSize) = do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc <- doCompile slug program
  fp   <- doMake slug cSrc (aQSize, eQSize)
  _    <- doVg slug fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
propCorrect :: Backend backend => TestName -> Program backend -> QC.Property
propCorrect tn program =
  QC.monadicIO $ mapM_ (propCorrectWithSize tn $ unsafeCoerce program) queueSizes

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
-- Sizes are give as an argument
propCorrectWithSize ::
  TestName -> (forall backend . Program backend) -> (Int, Int) -> QC.PropertyM IO ()
propCorrectWithSize tn program (aQSize, eQSize) = do
  slug <- QC.run $ getSlug tn
  reportSlug slug
  reportProgramOnFail slug program
  cSrc        <- doCompile slug program
  fp          <- doMake slug cSrc (aQSize, eQSize)
  _           <- doVg slug fp
  (_, out, _) <- doExec slug fp
  cTrace      <- doParseOutput slug out
  iTrace      <- doInterpret slug program (length $ lines out) (aQSize, eQSize)
  doCompareTraces slug iTrace cTrace

-- | Spec that ensures an SSM program compiles to valid C that compiles and runs
-- without memory errors, and behaves the same as the interpreter.
--
-- Used to build passing integration tests.
correctSpec :: String -> (forall backend . Backend backend => Program backend) -> H.Spec
correctSpec name p = do
  once $ H.prop "compiles" $ propCompiles tn p
  once $ H.prop "no memory errors" $ propValgrind tn p
  once $ H.prop "matches interpreter" $ propCorrect tn (p @C) -- FIXME
 where
  once = H.modifyMaxSuccess (const 1)
  tn   = NamedTest name

-- | Spec that ensures an SSM program compiles to valid C that compiles and runs
-- without memory errors, but behaves different from the interpreter.
--
-- Used to note discrepancies with the interpreter in the regression test suite.
-- Note that the description is still "matches interpreter" so that we can use
-- the same test name match clause (i.e., with HSpec's --match argument).
semanticIncorrectSpec :: String -> (forall backend . Backend backend => Program backend) -> H.Spec
semanticIncorrectSpec name p = do
  once $ H.prop "compiles" $ propCompiles tn p
  once $ H.prop "no memory errors" $ propValgrind tn p
  once $ H.prop "matches interpreter" $ QC.expectFailure $ propCorrect tn (p @C) -- FIXME
 where
  once = H.modifyMaxSuccess (const 1)
  tn   = NamedTest name

propSyntacticEquality :: String -> (forall backend . Backend backend => Program backend) -> (forall backend . Backend backend => Program backend) -> H.Spec
propSyntacticEquality name p1 p2 = do
  H.prop "produces correct syntax" $ QC.monadicIO $ do
    QC.monitor $ QC.whenFail $ do
      putStrLn "Program produce illegal syntax"
      putStrLn "program 1:"
      putStrLn $ show $ p1 @PrettyPrint
      putStrLn ""
      putStrLn "program 2:"
      putStrLn $ show $ p2 @PrettyPrint
    return $ p1 @PrettyPrint == p2
