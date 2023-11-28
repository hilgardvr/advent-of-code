module Main (main) where

import Lib (runapp)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
    f <- readFile $ head args
    let res = runapp f 
    print res
