
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
--    c <- testSingle $ nonterminate inputIntRef
--    case c of
--        Left err -> putStrLn err
--        Right _  -> putStrLn "test OK"

    quickCheck prop_correct

prop_correct :: Program -> Property
prop_correct (Program main) = monadicIO $ do
    res <- run $ testSingle main
    case res of
        Left s  -> do monitor (whenFail $ do
                        putStrLn "Test failed!"
                        putStrLn "Compiling and writing test file to fail.c..."
                        writeFile "fail.c" (compile True Nothing main)
                        putStrLn "Prettyprinting and writing test file to fail.ssm..."
                        writeFile "fail.ssm" (showSSM main)
                        putStrLn "writing the output traces to fail.out..."
                        writeFile "fail.out" s)
                      assert False
        Right _ -> return ()


{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: SSM () -> IO (Either String ())
testSingle program = do
    mtrace1   <- runCG program
    case mtrace1 of
        Just trace1 -> do let trace2 = take (length trace1) $ interpret program
                          if trace1 /= trace2
                              then return (Left (unlines (errorStr trace1 trace2)))
                              else return $ Right ()
        Nothing     -> return (Left "code generator failed")

errorStr :: Output -> Output -> [String]
errorStr xs ys = zipWith (\x y -> padTo xsmax x ++ padTo ysmax y ++ indicate x y) xs' ys'
  where
      xs'   = "code generator" : "--------------" : map show xs
      ys'   = "interpreter"    : "-----------" : map show ys
      xsmax = foldl (\acc str -> max acc (length str)) 0 xs'
      ysmax = foldl (\acc str -> max acc (length str)) 0 ys'

      padTo :: Int -> String -> String
      padTo n str = str ++ replicate (n - length str + 2) ' '

      indicate :: String -> String -> String
      indicate x y = if x /= y 
          then if "-" `isPrefixOf` x || "code" `isPrefixOf` x 
              then "" 
              else " <----- different" 
          else ""