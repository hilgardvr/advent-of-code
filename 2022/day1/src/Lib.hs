module Lib
    ( someFunc
    , runapp
    ) where

import Data.List (sort)

someFunc :: IO ()
someFunc = 
    putStrLn "Here" >>
    putStrLn "someFunc"

accumulator :: [String] -> Int -> Integer -> [(Int, Integer)]
accumulator [] e acc = [(e, acc)]
accumulator (h:t) e acc = 
    if h == ""
    then (e, acc) : accumulator t (e+1) 0
    else accumulator t e (acc + (read h :: Integer))

maxi :: [(Int, Integer)] -> (Int, Integer)
maxi is = foldr helper (0,0) is
    where 
        helper :: (Int, Integer) -> (Int, Integer) -> (Int, Integer)
        helper a b =
            if snd a > snd b
            then a 
            else b

runapp :: FilePath -> Int -> IO Integer
runapp fp i = do 
    f <- readFile fp
    let ls = accumulator (lines f) 1 0
    --putStrLn (show ls)
    --let maxcal = maxi ls
    return $ sum $ take i $ reverse . sort $ map snd ls
