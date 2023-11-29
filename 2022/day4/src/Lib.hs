module Lib
    ( someFunc
    , runapp
    ) where
import Data.List (elemIndex)
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

splitAtChar :: Char -> String -> Maybe (String, String)
splitAtChar c s = do
    i <- elemIndex c s
    Just $ (take i s, drop (i+1) s)

getRange :: String -> Maybe [Int]
getRange r =
    let 
        pr = splitAtChar '-' r
    in do
        (s,e) <- pr
        Just $ [read s :: Int .. read e :: Int]

lastElem :: [a] -> Maybe a
lastElem [] = error "Empty list"
lastElem [x] = Just x
lastElem (_:xs) = lastElem xs

contains :: [Int] -> [Int] -> Bool
contains xs ys =
    let 
        fx = head xs
        lx = lastElem xs
        fy = head ys
        ly = lastElem ys
    in
        (fx <= fy && lx >= ly) || (fy <= fx && ly >= lx)


overlaps :: [Int] -> [Int] -> Bool
overlaps [] _ = False
overlaps (x:xs) ys = 
    if elem x ys 
    then trace (show x ++ ":" ++ show ys) True 
    else overlaps xs ys
    

runapp :: [String] -> Maybe Int
runapp [] = Just 0
runapp (x:xs) = do
    pr <- splitAtChar ',' x
    e1 <- getRange $ fst pr
    e2 <- getRange $ snd pr
    if overlaps e1 e2
    then do
        c <- runapp xs
        Just (1 + c)
    else runapp xs
