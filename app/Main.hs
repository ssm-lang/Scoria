module Main where

import Frontend
import Fib
import NonTerminate
import Core
import Interpreter

main :: IO ()
main = do
    let output = interpret $ nonterminate inputIntRef--(myfib (int 13) (Ptr ("r", mkReference TInt)))
    putStrLn $ unlines output