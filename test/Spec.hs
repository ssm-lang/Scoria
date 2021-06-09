{-# LANGUAGE ScopedTypeVariables #-}
-- import           NonTerminate

import qualified Build                         as C
import qualified Output                        as O
import Report

import           LowCore                        ( Program )
import           LowGenerator                   ( ) -- for instance Arbitrary Program
import           LowPretty

import           Test.QuickCheck                ( Property
                                                , quickCheck
                                                , withMaxSuccess
                                                )
import           Test.QuickCheck.Monadic        ( monadicIO )

-- import           TestCases

-- timestamp <- round . (* 1000) <$> getPOSIXTime

-- | Tests that generated SSM programs compile successfully
propCompiles :: Program -> Property
propCompiles program = monadicIO $ do
  cSrc <- C.doCompile program
  C.doMake cSrc
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: Program -> Property
propValgrind program = monadicIO $ do
  cSrc <- C.doCompile program
  fp   <- C.doMake cSrc
  _    <- C.doVg fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
--
-- TODO: these nested case statements is exactly why Haskell has monads; figure
-- out how to rewrite this with monads. In particular, maybe try using
-- QuickCheck's PropertyM here?
propCorrect :: Program -> Property
propCorrect program = monadicIO $ do
  cSrc        <- C.doCompile program
  fp          <- C.doMake cSrc
  (_, out, _) <- C.doExec fp
  cTrace      <- O.doParseOutput out
  iTrace      <- O.doInterpret program (length $ lines out)
  O.doCompareTraces cTrace iTrace

-- | Entry point for test harness
main :: IO ()
main = do
  quickCheck (withMaxSuccess 1000 propValgrind)
  quickCheck (withMaxSuccess 1000 propCorrect)
  return ()
