module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    f <- readFile $ head args
    print $ runapp f 
