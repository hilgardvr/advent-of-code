module Lib
    ( someFunc
    , validApp
    , fewestApp
    ) where

import Debug.Trace (trace)
import Data.List

data Game = Game {
    gid :: Integer
    , samples :: [Sample]
} deriving (Show)

data Sample = Sample {
    red :: Integer
    , green :: Integer
    , blue :: Integer
} deriving (Show)

emptySample :: Sample
emptySample = Sample { red = 0, green = 0, blue = 0 }


someFunc :: IO ()
someFunc = putStrLn "someFunc"

parse :: Char -> String -> (String, String)
parse c s =  
    case elemIndex c s of
        Nothing -> error "No : found"
        Just i -> splitAt i s

splitAtAndRemove :: Char -> String -> [String]
splitAtAndRemove c [] = []
splitAtAndRemove c xs = 
    case elemIndex c xs of 
        Nothing -> [xs]
        Just i -> take i xs : splitAtAndRemove c (drop (i+1) xs)

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (== ' ')

readSample :: String -> Sample
readSample s =
    let 
        trm :: [String]
        trm = splitAtAndRemove ',' (trim s)
        prs :: [[String]]
        prs = map ((splitAtAndRemove ' ') . trim) trm

        helper :: [String] -> Sample -> Sample
        helper xs s =
            case head . tail $ xs of
                "red" -> s { red = (read (head xs) :: Integer) }
                "green" -> s { green = (read (head xs) :: Integer) }
                "blue" -> s { blue = (read (head  xs) :: Integer) }
                _ -> error $ "No color: " ++ (head . tail) xs
    in
        foldr (\e a -> helper e a) emptySample prs


parseGame :: [String] -> Game
parseGame s = 
    let
        gid = read $ drop 5 (head s) :: Integer
        samples = head . tail $ s
        parsedSamples :: [String]
        parsedSamples = splitAtAndRemove ';' samples
        dataSamples = map readSample parsedSamples
    in
        Game {
            gid = gid,
            samples = dataSamples
        }

isValidGame :: Sample -> Game -> Bool
isValidGame s g =
    let 
        spls = samples g

        isValidSample :: Sample -> Bool
        isValidSample s' = red s' <= red s && green s' <= green s && blue s' <= blue s
    in 
        all id (map isValidSample spls)
    

validSample :: Sample
validSample = Sample {
    red = 12
    , green = 13
    , blue = 14
}

validApp :: String -> Integer
validApp s = 
    let
        ls = lines s
        games = map (splitAtAndRemove ':') ls
        readGames = map parseGame games
        validGames = filter (isValidGame validSample) readGames
    in
        trace (show validGames) (sum (map gid validGames))

maxSamples :: [Sample] -> Sample
maxSamples xs =
    foldr (\e a -> 
        Sample {
            red = max (red e) (red a)
            , green = max (green e) (green a)
            , blue = max (blue e) (blue a)
        }) emptySample xs

powerSample :: Sample -> Integer
powerSample s = red s * green s * blue s

fewestApp :: String -> Integer
fewestApp s = 
    let
        ls = lines s
        games = map (splitAtAndRemove ':') ls
        readGames = map parseGame games
        maxes = map (maxSamples . samples) readGames
        sums = map powerSample maxes
    in
        trace (show (length sums) ++ show sums) (sum sums)
