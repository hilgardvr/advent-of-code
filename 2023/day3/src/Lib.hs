module Lib
    ( someFunc
    , partNumbers
    , gearsNumbers
    ) where
import Data.Char (isNumber)
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Coord = Coord {
    x :: Int
    , y :: Int
} deriving (Show)

instance Eq Coord where
    a == b = (x a == x b) && (y a == y b)

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

symbolIndexLine :: Int -> Int -> String -> [(Coord, Char)]
symbolIndexLine x y [] = []
symbolIndexLine x y xs = 
    if (not . isNumber $ head xs) && head xs /= '.'
    then (Coord {x = x, y = y}, head xs) : symbolIndexLine (x + 1) y (drop 1 xs) 
    else symbolIndexLine (x + 1) y (drop 1 xs) 
    
numIndexParser :: Int -> [String] -> [(Coord, String)]
numIndexParser _ [] = []
numIndexParser y' (x':xs) = numIndexLine 0 y' x' ++ numIndexParser (y'+1) xs

symbolIndexParser :: Int -> [String] -> [(Coord, Char)]
symbolIndexParser _ [] = []
symbolIndexParser y' (x':xs) = symbolIndexLine 0 y' x' ++ symbolIndexParser (y'+1) xs

adjacents :: Coord -> (Coord, String) -> [Coord]
adjacents m (c, n)  =
    let
        minx = max (0) (x c - 1)
        maxx = min (x c + length n) (x m)
        miny = max 0 (y c - 1)
        maxy = min (y c + 1) (y m)
        xs = [minx..maxx]
        ys = [miny..maxy]
    in
        [Coord {x = x', y = y'} | x' <- xs, y' <- ys ]

numsWithAdjacents :: (Coord) -> [(Coord, String)] -> [(Coord, Char)] -> [(Coord, String)]
numsWithAdjacents _ [] _ = []
numsWithAdjacents max (h:t) cs = 
    let 
        adjs = trace ("before: " ++ show cs) adjacents max h
        cCoords = trace ("adj: " ++ show adjs) map fst cs

        matches :: [Coord] -> Bool
        matches [] = False
        matches (h:t) = 
            if any (== h) adjs
            then True
            else matches t
    in
        if matches cCoords
        then h : numsWithAdjacents max t cs
        else numsWithAdjacents max t cs

gearsLine :: Int -> Int -> String -> [(Coord, Char)]
gearsLine _ _ [] = []
gearsLine x y xs = 
    if head xs == '*'
    then (Coord {x = x, y = y}, '*') : gearsLine (x + 1) y (drop 1 xs) 
    else gearsLine (x + 1) y (drop 1 xs) 

gearsIndexParser :: Int -> [String] -> [(Coord, Char)]
gearsIndexParser _ [] = []
gearsIndexParser y' (x':xs) = gearsLine 0 y' x' ++ gearsIndexParser (y'+1) xs


partNumbers :: String -> Int
partNumbers s = 
    let
        ls = lines s
        numXs = (length $ head ls) - 1
        numYs = (length ls) - 1
        max = Coord {x = numXs, y = numYs}
        nis = numIndexParser 0 ls
        sis = symbolIndexParser 0 ls
        res = numsWithAdjacents max nis sis
        total = foldr (\e a -> (read (snd e) :: Int) + a) 0 res
    in
        trace (show res) total

gearsNums :: (Coord) -> [(Coord, String)] -> (Coord, Char) -> Int
gearsNums max nums' gear = 
    let 
        nums = trace ("here") numsWithAdjacents max nums' [gear]

        ratio :: [(Coord, String)] -> Int
        ratio ns = foldr (\e a -> (read (snd e) :: Int) * a) 1 ns
    in
        if length nums == 2
        then trace ("done:" ++ show nums) (ratio nums)
        else trace ("none") 0


gearsNumbers :: String -> Int
gearsNumbers s = 
    let
        ls = lines s
        numXs = (length $ head ls) - 1
        numYs = (length ls) - 1
        max' = Coord {x = numXs, y = numYs}
        nis = numIndexParser 0 ls
        gis = trace (show nis) gearsIndexParser 0 ls
        ratios = trace (show gis) map (gearsNums max' nis) gis
    in
        sum ratios

