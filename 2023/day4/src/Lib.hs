module Lib
    ( someFunc
    , runapp
    , cardwinapp
    ) where
import Data.List (elemIndex)
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Game = Game {
    gid :: Int
    , winNumbers :: [Int]
    , myNumbers :: [Int]
} deriving (Show, Read)

splitAtAndRemove :: Char -> String -> [String]
splitAtAndRemove c [] = []
splitAtAndRemove c xs = 
    case elemIndex c xs of 
        Nothing -> [xs]
        Just i -> take i xs : splitAtAndRemove c (drop (i+1) xs)

trim :: String -> String
trim = f . f 
    where f = reverse . dropWhile (==' ')

parseInputLine :: String -> Game
parseInputLine s =
    let
        g = splitAtAndRemove ':' s
        filteredGame = filter (/="") g
        gnUnfiltered =  (splitAtAndRemove ' ' (head filteredGame))
        gn = trace ("uifiltered:" ++ show gnUnfiltered) head.tail $ filter (/="") gnUnfiltered
        nums = splitAtAndRemove '|' ((head . tail)  g)
        win = filter (/="") $ splitAtAndRemove ' ' $ trim (head nums)
        my = filter (/="") $ splitAtAndRemove ' ' $ trim ((head. tail) nums)
        gameNum = trace (show gn) read gn :: Int
        wins :: [Int]
        wins = map read win
        mine :: [Int]
        mine = map read my
    in
        --trace ("gn: " ++ (show gameNum) ++ "\nwin: " ++ (show wins) ++ "\nmy: " ++ (show mine)) 
            Game { gid = gameNum, winNumbers =  wins, myNumbers = mine }
        
numWins :: Game -> Int
numWins (Game {gid = _, winNumbers = _, myNumbers = []}) = 0
numWins (Game {gid = g, winNumbers = w, myNumbers = (h:t)}) = 
    if elem h w
    then 1 + numWins Game { gid = g, winNumbers = w, myNumbers = t}
    else numWins Game {gid = g, winNumbers = w, myNumbers = t}
    
winsToPoints :: Int -> Int
winsToPoints w =
    if w <= 0
    then 0
    else 
        if w == 1 
        then 1
        else winsToPoints (w-1) * 2

newCards :: [Game] -> [Game]
newCards [] = []
newCards gs = 
    let 
        wns = numWins $ head gs
        gi = gid $ head gs

        addGames :: Int -> Int -> [Game]
        addGames 0 _  = []
        addGames w gid' =
            let 
                gameIdToAdd = w + gid'
                gameToAdd = head $ filter (\e -> gid e == gameIdToAdd) gs
            in 
                gameToAdd : addGames (w -1) gid'

        added = addGames wns gi
    in
        head gs : newCards (added ++ tail gs)
            

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

cardwinapp :: String -> Int
cardwinapp s = 
    let 
        ls = lines s
        parsed = map parseInputLine ls
        allCards = newCards parsed
    in
        --trace (show parsed) 
            (length allCards)
