import Lib (runapp, runapp2)



main :: IO ()
main = do
    f <- readFile "test.txt"
    let res = runapp f 
    print res
    let res2 = runapp2 f
    case res2 of
        Nothing -> putStrLn "FAIL: NOTHING"
        Just x -> putStrLn $ if x == 70 then "PASS: 70" else "FAIL: " ++ show res2
    f2 <- readFile "input.txt"
    let ans = runapp2 f2
    case ans of
        Nothing -> putStrLn "FAIL: NOTHING"
        Just x -> putStrLn $ "ans: " ++ show x
