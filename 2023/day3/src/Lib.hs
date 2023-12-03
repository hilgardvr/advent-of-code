module Lib
    ( someFunc
    , partNumbers
    ) where
import Data.Char (isNumber)
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Coord = Coord {
    x :: Int
    , y :: Int
} deriving (Show)

numIndexLine :: Int -> Int -> String -> [(Coord, String)]
numIndexLine x y [] = []
numIndexLine x y xs = 
    if isNumber $ head xs
    then 
        let
            n = takeWhile isNumber xs
            l = length n
            c = Coord { x = x, y = y }
        in
            (c, n) : numIndexLine (x + l) y (drop l xs) 
    else 
        let
            n = takeWhile (not . isNumber) xs
            l = length n
        in
            numIndexLine (x + l) y (drop l xs) 
    
numIndexParser :: Int -> [String] -> [(Coord, String)]
numIndexParser _ [] = []
numIndexParser y (x:xs) = 
    numIndexLine 0 y x ++ numIndexParser (y+1) xs
    


partNumbers :: String -> Integer
partNumbers s = 
    let
        ls = lines s
        nis = numIndexParser 0 ls
    in
        trace (show nis) 0


