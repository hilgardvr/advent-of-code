module Lib
    ( someFunc
    , runapp
    ) where
import Debug.Trace (trace)

data Op = A | B | C deriving (Read, Show)
data Me = X | Y | Z deriving (Read, Show)

val :: Me -> Int
val X = 1
val Y = 2
val Z = 3

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseLines :: [String] -> [(Op, Me)]
parseLines [] = []
parseLines (h:t) =
    let 
        w = words h
    in
        if length w /= 2
        then trace ("Length != 2" ++ show w) parseLines t
        else 
            let 
                o = read (head w) :: Op
                m = read (w!!1) :: Me
            in
                (o, m) : parseLines t

calc :: [(Op, Me)] -> Int
calc [] = 0
calc (h:t) = 
    let 
        o = fst h
        m = snd h

        --res :: Op -> Me -> Int
        --res A X = 3
        --res A Y = 6
        --res A Z = 0
        --res B X = 0
        --res B Y = 3
        --res B Z = 6
        --res C X = 6
        --res C Y = 0
        --res C Z = 3
        res :: Op -> Me -> Int
        res A X = 3
        res A Y = 4
        res A Z = 8
        res B X = 1
        res B Y = 5
        res B Z = 9
        res C X = 2
        res C Y = 6
        res C Z = 7
    in
        res o m + calc t

    

runapp :: String -> Int
runapp f = 
    let 
        parsed = parseLines (lines f)
    in
        calc parsed
        
        
        
