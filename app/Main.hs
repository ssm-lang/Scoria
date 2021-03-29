module Main where

import Data.List

import Frontend
import Fib
import NonTerminate
import Core
import Interpreter

main :: IO ()
main = do
    --let output = interpret $ nonterminate inputIntRef
    let output = interpret $ myfib (int 13) inputIntRef
    putStrLn $ intercalate "\n" $ map show output