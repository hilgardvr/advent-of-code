module Lib
    ( someFunc
    , runapp
    , replaceText
    ) where
import Data.Char (isDigit)
import qualified Data.Text as T
import Debug.Trace (trace)
import Data.String (IsString(fromString))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

safeLast :: String -> Char
safeLast [] = error "Empty list"
safeLast (h:[]) = h
safeLast (_:t) = safeLast t

fromSpelledString :: [(String, String)]
fromSpelledString = 
    [ ("one", "1") 
    , ("two", "2") 
    , ("three", "3") 
    , ("four", "4") 
    , ("five", "5") 
    , ("six", "6") 
    , ("seven", "7") 
    , ("eight", "8") 
    , ("nine", "9")
    ]

--replaceText :: String -> String
--replaceText s = foldr (\e a -> helper e a) s fromSpelledString
--    where 
--        helper :: (String, String)  -> String -> String
--        helper (sp, num) a = T.unpack $ T.replace (T.pack sp) (T.pack num) (T.pack a)
--
replaceText :: String -> String
replaceText [] = ""
replaceText xs = 
    let
        helper :: (String, String) -> String -> String
        helper _ [] = ""
        helper (ss,n) s = 
            if (take (length ss) s) == ss
            then n ++ (drop (length ss) s)
            else s

        replaced = foldr (\e a -> helper e a) xs fromSpelledString
    in
        if replaced == xs
        then head xs : replaceText (tail xs)
        else replaceText replaced

firstLastInteger :: String -> Integer
firstLastInteger s = 
    let 
        f = head s
        l = safeLast s
    in
        read [f,l] :: Integer 


runapp :: String -> Integer
runapp f = 
    let 
        ls = lines f
        rp = map replaceText ls
        ds = map (filter isDigit) rp
        fsi = map firstLastInteger ds
    in
        trace ("ls: " ++ show ls ++ "\nrp: " ++ show rp ++ "\nds: " ++ show ds ++ "\nfsi: " ++ show fsi) sum fsi
        

