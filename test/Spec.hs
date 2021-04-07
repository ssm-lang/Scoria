
import Evaluation
import Frontend2
import Interpreter2
import Fib2
import Core2
import Trace2
import Generator2
import CodeGen2

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = quickCheck prop_correct

prop_correct :: Program -> Property
prop_correct (Program main) = monadicIO $ do
    res <- run $ testSingle main
    case res of
        Left s  -> do monitor (whenFail $ writeFile "/home/robert/fail.c" (compile True Nothing main) >> putStrLn s)
                      assert False
        Right _ -> return ()


{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: SSM () -> IO (Either String ())
testSingle program = do
    out1   <- runCG program
    let trace2 = interpret program
    let trace1 = mkTrace out1
    if trace1 /= trace2
        then return $ Left $ "--* output from c *--\n" ++ out1 ++
                             "\n\n--* output from interpreter --*\n" ++ (unlines (map show trace2))
        else return $ Right ()