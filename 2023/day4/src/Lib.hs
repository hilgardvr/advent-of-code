module Lib
    ( someFunc
    , runapp
    ) where
import Data.List (elemIndex)
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


splitAtAndRemove :: Char -> String -> [String]
splitAtAndRemove c [] = []
splitAtAndRemove c xs = 
    case elemIndex c xs of 
        Nothing -> [xs]
        Just i -> take i xs : splitAtAndRemove c (drop (i+1) xs)

trim :: String -> String
trim = f . f 
    where f = reverse . dropWhile (==' ')

parseInputLine :: String -> (Int, [Int], [Int])
parseInputLine s =
    let
        g = splitAtAndRemove ':' s
        gn =  (head . tail) (splitAtAndRemove ' ' (head g))
        nums = splitAtAndRemove '|' ((head . tail)  g)
        win = filter (/="") $ splitAtAndRemove ' ' $ trim (head nums)
        my = filter (/="") $ splitAtAndRemove ' ' $ trim ((head. tail) nums)
        gameNum = read gn :: Int
        wins :: [Int]
        wins = map read win
        mine :: [Int]
        mine = map read my
    in
        --trace ("gn: " ++ (show gameNum) ++ "\nwin: " ++ (show wins) ++ "\nmy: " ++ (show mine)) 
            (gameNum, wins, mine)
        
firstTriple :: (a,b,c) -> a
firstTriple (a,_,_) = a

numWins :: (Int, [Int], [Int]) -> Int
numWins (_, _, []) = 0
numWins (g, w, (h:t)) = 
    if elem h w
    then 1 + numWins (g,w,t)
    else numWins (g,w,t)
    
winsToPoints :: Int -> Int
winsToPoints w =
    if w <= 0
    then 0
    else 
        if w == 1 
        then 1
        else winsToPoints (w-1) * 2

runapp :: String -> Int
runapp s = 
    let 
        ls = lines s
        parsed = map parseInputLine ls
        wins = map numWins parsed
        points = map (winsToPoints) wins
    in
        trace (show wins ++ "\npoints: " ++ show points) 
            (sum points)
