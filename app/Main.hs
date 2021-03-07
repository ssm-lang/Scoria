module Main where

import Frontend
import Fib
import Core
import Interpreter

main :: IO ()
main = interpret (myfib (int 13) "r") [("r", Lit (LInt 0))] --putStrLn "I am not implemented yet"
