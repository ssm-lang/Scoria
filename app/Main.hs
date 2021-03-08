module Main where

import Frontend
import Fib
import Core
import Interpreter

main :: IO ()
main = interpret (myfib (int 13) (Ptr ("r", mkReference TInt))) [("r", Lit TInt (LInt 0))] --putStrLn "I am not implemented yet"
