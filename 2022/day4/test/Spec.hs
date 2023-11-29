import Lib (runapp)
main :: IO ()
main = do 
    f <- readFile "test.txt"
    let r = runapp $ lines f
    putStrLn $ if r == 2 then "PASS" else "FAIL"

