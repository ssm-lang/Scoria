module Main where

import BoxTest

main :: IO ()
main = interpret (myfib (int 13) "r") [("r", Lit (LInt 0))] --putStrLn "I am not implemented yet"
