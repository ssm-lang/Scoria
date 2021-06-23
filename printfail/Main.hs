module Main where

import System.Environment

import SSM.Pretty
import SSM.Core.LowSyntax

main :: IO ()
main = do
    args <- getArgs --readFile "fail.ssm"
    case args of
        [] -> undefined
        [file] -> do x <- readFile file
                     let program = read x :: Program
                     putStrLn $ prettyProgram program