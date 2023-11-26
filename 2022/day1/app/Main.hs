module Main (main) where

import Lib (runapp)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    --putStrLn $ show args
    let c = read (head . tail $ args) :: Int
    runapp (head args) c >>= print
