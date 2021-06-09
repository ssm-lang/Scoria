import qualified Build                         as C
import qualified Output                        as O
import           Report                         ( Slug(..)
                                                , getTimestamp
                                                , reportProgramOnFail
                                                , reportSlug
                                                )

import           LowCore                        ( Program )
import           LowGenerator                   ( ) -- instance Arbitrary Program

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

-- import           TestCases

-- | Tests that generated SSM programs compile successfully
propCompiles :: Slug -> Program -> QC.Property
propCompiles sl program = QC.monadicIO $ do
  reportSlug sl
  reportProgramOnFail sl "quickcheckgen.ssm" program
  cSrc <- C.doCompile sl program
  C.doMake sl cSrc
  return ()

-- | Tests an SSM program by evaluating it under valgrind.
propValgrind :: Slug -> Program -> QC.Property
propValgrind sl program = QC.monadicIO $ do
  reportSlug sl
  reportProgramOnFail sl "quickcheckgen.ssm" program
  cSrc <- C.doCompile sl program
  fp   <- C.doMake sl cSrc
  _    <- C.doVg sl fp
  return ()

-- | Tests an SSM program by evaluating both the interpreter and running the
-- compiled C code and comparing the output.
--
-- TODO: these nested case statements is exactly why Haskell has monads; figure
-- out how to rewrite this with monads. In particular, maybe try using
-- QuickCheck's PropertyM here?
propCorrect :: Slug -> Program -> QC.Property
propCorrect sl program = QC.monadicIO $ do
  reportSlug sl
  reportProgramOnFail sl "quickcheckgen.ssm" program
  cSrc        <- C.doCompile sl program
  fp          <- C.doMake sl cSrc
  _           <- C.doVg sl fp
  (_, out, _) <- C.doExec sl fp
  cTrace      <- O.doParseOutput sl out
  iTrace      <- O.doInterpret sl program (length $ lines out)
  O.doCompareTraces sl cTrace iTrace
  QC.assert False

-- | Entry point for test harness
main :: IO ()
main = do
  sl <- getTimestamp
  QC.quickCheck (QC.withMaxSuccess 1000 $ propCorrect sl)
  return ()
