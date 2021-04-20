{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.List

import Frontend
import Fib
import NonTerminate
import Core
import Trace
import qualified LowCore as LC
import qualified Interpreter as I1
import qualified LowInterpreter as I2
import Data.Time ( diffUTCTime, getCurrentTime )
import System.IO

main :: IO ()
main = do
    putStrLn "old interpreter time..."
    start <- getCurrentTime
    let output1 = I1.interpret $ myfib (int 15) inputIntRef
    hPutStrLn stderr $ show output1
    end <- getCurrentTime
    print $ diffUTCTime end start
    putStrLn "new interpreter time..."
    start2 <- getCurrentTime
    let output2 = I2.interpret $ LC.transpile $ myfib (int 15) inputIntRef
    hPutStrLn stderr $ show output1
    end2 <- getCurrentTime
    print $ diffUTCTime end2 start2
--let output = interpret $ nonterminate inputIntRef
--    let output = I2.interpret $ LC.transpile $ myfib (int 15) inputIntRef
--    putStrLn $ intercalate "\n" $ map show output