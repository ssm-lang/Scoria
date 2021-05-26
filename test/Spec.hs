{-# LANGUAGE ScopedTypeVariables #-}
import Evaluation
--import Frontend
--import Interpreter
import LowCore hiding (main)
import Trace
import LowGenerator
import LowCodeGen
import Frontend
--import LowPretty

import Fib
import NonTerminate

import Control.Exception hiding (assert)
import System.Timeout
import Data.IORef
import Data.List
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Unsafe
import Buggy
import TestCases

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x



main :: IO ()
main = do
--    x <- readFile "fail.ssm"
--    let program = read x :: Program
--    r <- testSingle program (Just 10000)
--    print r
--    return ()
--    quickCheck $ verboseShrinking $ prop_test_dummy
--      regression_test
--      r <- testSingle singlecase limit
--      case r of
--          Good -> putStrLn "Good"
--          Bad t1 t2 -> putStrLn $ errorStr t1 t2
--          _         -> return ()
--      return ()
--    setupTestDir
--    quickCheck (withMaxSuccess 15000 $ verboseShrinking prop_correct)
--    removeTestDir
--    setupTestDir 
--    quickCheck (withMaxSuccess 5000 $ prop_compiles_ok)
--    removeTestDir 
    setupTestDir 
    quickCheck (withMaxSuccess 1000 $ prop_valgrind_ok)
    removeTestDir 

data Dummy = Dummy Program deriving Show

instance Arbitrary Dummy where
    arbitrary = return $ Dummy singlecase
    shrink (Dummy p) = map Dummy $ shrink p

prop_test_dummy :: Dummy -> Property
prop_test_dummy (Dummy p) = prop_correct p

limit :: Maybe Int
limit = Just 7500

regression_test :: IO ()
regression_test = do
    sequence_ $ flip map testcases $ \p -> do
        r <- testSingle p limit
        case r of
            Good -> putStrLn "good"
            o -> putStrLn $ "bad" --show o

prop_correct :: Program -> Property
prop_correct p = whenFail (do writeFile "timeout.c" $ compile_ True limit p
                              writeFile "timeout.ssm" $ show p) $
      within 15000000 $ monadicIO $ do
    res <- run $ testSingle p limit
    case res of
        Good               -> return ()
        Bad t1 t2          -> do monitor (whenFail $ do
                                            putStrLn "************"
                                            putStrLn "Test failed!"
                                            putStrLn "************\n"
                                            putStrLn "writing fail.c..."
                                            writeFile "fail.c" (compile_ True limit p)
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
                                            writeFile "comperror.c" $ compile_ True limit p
                                            putStrLn "writing comperror.ssm"
                                            writeFile "comperror.ssm" $ show p)
                                 assert False
        ExecutionError s   -> do monitor (whenFail $ do
                                            putStrLn   "***************"
                                            putStrLn $ "execution-error"
                                            putStrLn   "***************\n"
                                            putStrLn s
                                            putStrLn "writing execerror.c"
                                            writeFile "execerror.c" $ compile_ True limit p
                                            putStrLn "writing execerror.ssm"
                                            writeFile "execerror.ssm" $ show p)
                                 assert False
        ParseError s       -> do monitor (whenFail $ do
                                            putStrLn   "***********"
                                            putStrLn $ "parse-error"
                                            putStrLn   "***********\n"
                                            putStrLn s
                                            putStrLn "writing parseerror.c"
                                            writeFile "parseerror.c" $ compile_ True limit p
                                            putStrLn "writing parseerror.ssm"
                                            writeFile "parseerror.ssm" $ show p
                                            putStrLn "writing parseerror.trace"
                                            writeFile "parseerror.trace" s)
                                 assert False

-- myafter prop. Given any program, convert all after statements to
-- use myafter and make sure that they are working as they should.

-- valgrind prop
-- for every generated program, valgrind will be happy
prop_valgrind_ok :: Program -> Property
prop_valgrind_ok p = monadicIO $ do
    b <- run $ runCGValgrind p limit
    assert b

-- compile prop
-- for every generated program, compilation will be successful
prop_compiles_ok :: Program -> Property
prop_compiles_ok p = monadicIO $ do
    b <- run $ runCGCompilation p
    assert b

{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: Program -> Maybe Int -> IO Report
testSingle program mi = do
    report1   <- runCG program mi
    case report1 of
        Generated trace1 -> do trace2 <- safeInterpreter program (length trace1)
                               if not (trace1 == trace2)
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

safeInterpreter :: Program -> Int -> IO Output
safeInterpreter p i = do
    timeoutEval i $ runInterpreter p

timeoutEval :: Int -> Output -> IO Output
timeoutEval i xs = do
    ref <- newIORef []
    xs' <- timeout 15000000 $ try $ eval xs i ref
    case xs' of
        Just (Left (e :: SomeException)) -> case show e of
            v | "eventqueue full" `isPrefixOf` v -> modifyIORef ref (EventQueueFull :)
              | "negative depth"  `isPrefixOf` v -> modifyIORef ref (NegativeDepth :)
              | "bad after"       `isPrefixOf` v -> modifyIORef ref (BadAfter :)
              | "contqueue full"  `isPrefixOf` v -> modifyIORef ref (ContQueueFull :)
            _ -> modifyIORef ref (Crash :)
        _ -> return ()
    reverse <$> readIORef ref

eval :: [a] -> Int -> IORef [a] -> IO ()
eval [] _ _       = return ()
eval _ 0 _        = return ()
eval (x:xs) i ref = do
    y <- evaluate x
    modifyIORef ref (y :)
    eval xs (i - 1) ref