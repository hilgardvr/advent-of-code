module Lib
    ( someFunc
    , runapp
    , runapp2
    ) where
import Debug.Trace (trace)
import Data.Char (isUpper, ord)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

removeDupes :: String -> String
removeDupes [] = []
removeDupes (x:xs) = x : removeDupes (filter (/=x) xs)

findDupes :: String -> String -> String
findDupes [] _ = []
findDupes (h:t) xs =
    if elem h xs
    then h : findDupes t xs
    else findDupes t xs

split :: String -> (String, String)
split s =
    let 
        len = length s
    in
        if even len
        then 
            let size = len `div` 2
            in splitAt size s --(take size s, drop size s)
        else trace "Found nothing" ("", "")

toPriority :: Char -> Int
toPriority c =
    if isUpper c 
    then ord c - 64 + 26
    else ord c - 96
                
runapp :: String -> Int
runapp f =
    let 
        lns :: [String]
        lns = lines f
        spl :: [(String, String)]
        spl = map split lns
        dup :: [String]
        dup = map (uncurry findDupes) spl
        cln :: [String]
        cln = map removeDupes dup
        pri :: [Int]
        pri = map toPriority $ concat cln
    in
        trace ("res: " ++ show pri) $ sum pri

batchBadge :: [String] -> String -> String
batchBadge [] res = res
batchBadge (h:t) res  = 
    let
        dups :: String
        dups = removeDupes (findDupes h res)
    in
        batchBadge t dups
        

processBadge :: [String] -> Int
processBadge [] = 0
processBadge ls =
    let
        start :: [String]
        start = take 3 ls

        end :: [String]
        end = drop 3 ls

        dups = batchBadge start (head start)

    in
        toPriority (head dups) + processBadge end
    

runapp2 :: String -> Maybe Int
runapp2 f = 
    let ln = lines f
    in 
        if (length ln `mod` 3) /= 0
        then Nothing
        else Just $ processBadge ln

