import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC
import qualified Test.Ssm.Prop                 as T

import           Test.Hspec                     ( describe
                                                , hspec
                                                )
import           Test.Hspec.QuickCheck          ( modifyMaxSuccess
                                                , prop
                                                )

main :: IO ()
main = hspec $ do
  describe "Random program" $ do

    modifyMaxSuccess (const 10)
      $ prop "compiles to a binary (via C)"
      $ T.propCompiles T.RandomTest

    modifyMaxSuccess (const 10)
      $ prop "compiles and runs without memory errors"
      $ T.propValgrind T.RandomTest

    modifyMaxSuccess (const 10)
      $ prop "compiles and runs according to interpreter"
      $ T.propCorrect T.RandomTest
