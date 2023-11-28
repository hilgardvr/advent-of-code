import Lib

calcBasic :: String -> IO ()
calcBasic s = do
    let r = runapp s
    putStrLn $ show r
    putStrLn $ if r == 12 then "PASS" else "FAIL"

calcFromFile :: IO ()
calcFromFile = do
    f <- readFile "test.txt"
    calcBasic f


main :: IO ()
main = do
    calcBasic "A Y\nB X\nC Z" 
    calcFromFile
    --f <- readFile "test.txt"
