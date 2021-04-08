
import Evaluation
import Frontend
import Interpreter
import Core
import Trace
import Generator
import CodeGen
import Pretty

import Fib
import NonTerminate

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = do
--    c <- testSingle $ myfib (int 13) inputIntRef
    c <- testSingle $ nonterminate inputIntRef
    case c of
        Left err -> putStrLn err
        Right _  -> putStrLn "test OK"

--    quickCheck prop_correct

prop_correct :: Program -> Property
prop_correct (Program main) = monadicIO $ do
    res <- run $ testSingle main
    case res of
        Left s  -> do monitor (whenFail $ do
                        putStrLn "Test failed!"
                        putStrLn "Compiling and writing test file to fail.c..."
                        writeFile "/home/robert/fail.c" (compile True Nothing main)
                        putStrLn "Prettyprinting and writing test file to fail.ssm"
                        writeFile "/home/robert/fail.ssm" (showSSM main))
                      assert False
        Right _ -> return ()


{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: SSM () -> IO (Either String ())
testSingle program = do
    putStrLn "running code generator"
    out1   <- runCG program
    let trace1 = mkTrace out1
    putStrLn $ "running interpreter, need " ++ show (length trace1) ++ " lines of output"
    let trace2 = take (length trace1) $ interpret program
    if trace1 /= trace2
        then return (Left (unlines (printDifferences trace1 trace2 0)))
        else return $ Right ()

printDifferences :: Output -> Output -> Int -> [String]
printDifferences [] _ _ = []
printDifferences _ [] _ = []
printDifferences (x:xs) (y:ys) i = if x == y
    then printDifferences xs ys (i+1)
    else let line = concat ["entry ", show i, " was different: ", show x, " ", show y ]
         in line : printDifferences xs ys (i+1)