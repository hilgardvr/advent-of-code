import Lib

main :: IO ()
main = do
    t1 <- runapp "input.txt" 1
    putStrLn $ if t1 == 24000 then "PASS" else "FAIL"
    t2 <- runapp "input.txt" 3
    putStrLn $ if t2 == 45000 then "PASS" else "FAIL"
    return ()
