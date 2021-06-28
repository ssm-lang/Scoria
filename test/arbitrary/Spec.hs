import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC
import qualified Test.Ssm.Prop                 as T

import           Test.Hspec                     ( describe
                                                , hspec
                                                )
import           Test.Hspec.QuickCheck          ( modifyMaxSuccess
                                                , prop
                                                )

-- | Hspec entry point for arbitrary quickcheck test.
--
-- To specify the number of random programs to test from the command line,
-- adjust the maxSuccess parameters with the -a or --qc-max-success arguments.
-- For instance, invoking from stack:
--
-- > stack test --test-arguments='-a 420'
--
main :: IO ()
main = hspec $ do
  describe "Random program" $ do

--    prop "compiles to a binary (via C)"
--      $ T.propCompiles T.RandomTest

--    prop "compiles and runs without memory errors"
--      $ T.propValgrind T.RandomTest

    prop "compiles and runs according to interpreter"
      $ T.propCorrect T.RandomTest
