{-# LANGUAGE ScopedTypeVariables #-}
import Evaluation
--import Frontend
--import Interpreter
import LowCore hiding (main)
import Trace
import LowGenerator
import LowCodeGen
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

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x



main :: IO ()
main = do
    quickCheck (within 5000000 $ verboseShrinking prop_correct)
--    quickCheck (verboseShrinking prop_valgrind_ok)

prop_correct :: Program -> Property
prop_correct p = monadicIO $ do
    res <- run $ testSingle p (Just 10000)
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

-- myafter prop. Given any program, convert all after statements to
-- use myafter and make sure that they are working as they should.

-- valgrind prop
-- for every generated program, valgrind will be happy

prop_valgrind_ok :: Program -> Property
prop_valgrind_ok p = monadicIO $ do
    b <- run $ runCGValgrind p (Just 10000)
    assert b

{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: Program -> Maybe Int -> IO Report
testSingle program mi = do
    report1   <- runCG program mi
    case report1 of
        Generated trace1 -> do trace2 <- take (length trace1) <$> safeInterpreter program
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


safeInterpreter :: Program -> IO Output
safeInterpreter p = do
    timeoutEval $ runInterpreter p

timeoutEval :: Output -> IO Output
timeoutEval xs = do
    ref <- newIORef []
    xs' <- timeout 5000000 $ try $ eval xs ref
    case xs' of
        Just (Left (e :: SomeException)) -> modifyIORef ref (Crash :)
        _ -> return ()
    reverse <$> readIORef ref

eval :: [a] -> IORef [a] -> IO ()
eval [] ref     = return ()
eval (x:xs) ref = do
    y <- evaluate x
    modifyIORef ref (y :)
    eval xs ref
    
{-
    case ys of
        Left (e :: SomeException) -> return () -- TODO signal error 
        Right []     -> return ()
        Right (x:xs) -> do y <- try $ evaluate x
                           case y of
                               Left (e :: SomeException) -> return () -- TODO signal error
                               Right y' -> do ys <- eval xs
                                              modifyIORef (y' :) ref
-}

{-

safeInterpreter :: Program -> IO Output
safeInterpreter p = do
    ref <- newIORef []
    h   <- async $ tout ref p
    wait h
    readIORef ref
  where
      loop :: Output -> IORef Output -> IO ()
      loop [] _   = return ()
      loop xs ref = do
          catch (do modifyIORef ref (\res -> res ++ take 10 xs)
                    loop (drop 10 xs) ref)
                (\(e :: SomeException) -> return ())
        
      tout ref p = do timeout 500000 $ let xs = runInterpreter p
                                       in loop xs ref
                      return ()
                      -}

{-

forkafter:
  - forkafter måste ha dummy parent
  - forkfter64 - forkandcontinue
  - Kolla om det går att göra det generellt, annars hårdkoda continue-and-fork-after-64

-}