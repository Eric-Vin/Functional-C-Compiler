module Main (main) where

import System.Environment

import Compiler

main :: IO ()
main = do
        args    <- getArgs
        params  <- processArgs args
        putStrLn $ show params
        compile params
