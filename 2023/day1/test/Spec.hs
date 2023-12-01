import Lib (runapp)

t1 :: IO ()
t1 = do 
    putStrLn "START : TEST 1"
    f <- readFile "test.txt"
    let r = runapp f
    if r == 142 
    then putStrLn "PASS"
    else putStrLn $ "FAIL: " ++ (show r)
    putStrLn "END : TEST 1"

t2 :: IO ()
t2 = do 
    putStrLn "START : TEST 2"
    f <- readFile "test2.txt"
    let r = runapp f
    if r == 281
    then putStrLn "PASS"
    else putStrLn $ "FAIL: " ++ (show r)
    putStrLn "END : TEST 2"


main :: IO ()
main = t1 >> t2
    
