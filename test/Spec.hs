
import Evaluation
import Frontend
import Interpreter
import Fib
import Core
import Trace
import Generator
import CodeGen

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = quickCheck prop_correct
{-
    do
    r <- testSingle $ myfib (int 13) (Ptr ("r", Ref TInt))
    case r of
        Left err -> putStrLn err
        Right _  -> putStrLn "test OK" -}


prop_correct (Program main) = monadicIO $ do
    res <- run $ testSingle main
    case res of
        Left s  -> do monitor (whenFail $ writeFile "fail.c" (compile True Nothing main) >> putStrLn s)
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