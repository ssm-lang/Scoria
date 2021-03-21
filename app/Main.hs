module Main where

import Frontend
import Fib
import Core
import Interpreter

main :: IO ()
main = do
    output <- interpret' (myfib (int 13) (Ptr ("r", mkReference TInt)))
    putStrLn $ unlines output