
import Evaluation
import Frontend
--import Interpreter
import LowCore hiding (main)
import Trace
import LowGenerator
import LowCodeGen
--import LowPretty

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

    quickCheck (verboseShrinking prop_correct)

prop_correct :: Program -> Property
prop_correct p = monadicIO $ do
    res <- run $ testSingle p
    case res of
        Good               -> return ()
        Bad t1 t2          -> do monitor (whenFail $ do
                                            putStrLn "************"
                                            putStrLn "Test failed!"
                                            putStrLn "************\n"
                                            putStrLn "writing fail.c..."
                                            writeFile "fail.c" (compile_ True Nothing p)
                                            putStrLn "writing fail.ssm..."
                                            writeFile "fail.ssm" (show p)
                                            putStrLn "writing fail.out..."
                                            writeFile "fail.out" $ errorStr t1 t2)
                                 assert False
        Generated _        -> assert False
        CompilationError s -> do monitor (whenFail $ do
                                            putStrLn   "*****************"
                                            putStrLn $ "compilation-error"
                                            putStrLn   "*****************\n"
                                            putStrLn s
                                            putStrLn "writing comperror.c"
                                            writeFile "comperror.c" $ compile_ True Nothing p
                                            putStrLn "writing comperror.ssm"
                                            writeFile "comperror.ssm" $ show p)
                                 assert False
        ExecutionError s   -> do monitor (whenFail $ do
                                            putStrLn   "***************"
                                            putStrLn $ "execution-error"
                                            putStrLn   "***************\n"
                                            putStrLn s
                                            putStrLn "writing execerror.c"
                                            writeFile "execerror.c" $ compile_ True Nothing p
                                            putStrLn "writing execerror.ssm"
                                            writeFile "execerror.ssm" $ show p)
                                 assert False
        ParseError s       -> do monitor (whenFail $ do
                                            putStrLn   "***********"
                                            putStrLn $ "parse-error"
                                            putStrLn   "***********\n"
                                            putStrLn s
                                            putStrLn "writing parseerror.c"
                                            writeFile "parseerror.c" $ compile_ True Nothing p
                                            putStrLn "writing parseerror.ssm"
                                            writeFile "parseerror.ssm" $ show p)
                                 assert False

{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: Program -> IO Report
testSingle program = do
    report1   <- runCG program
    case report1 of
        Generated trace1 -> do let trace2 = take (length trace1) $ runInterpreter program
                               if trace1 /= trace2
                                   then return $ Bad trace1 trace2
                                   else return $ Good
        otherwise      -> return otherwise

errorStr :: Output -> Output -> String
errorStr xs ys = unlines $ zipWith (\x y -> padTo xsmax x ++ padTo ysmax y ++ indicate x y) xs' ys'
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