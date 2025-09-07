import Data.Char

adder :: Int -> IO ()
adder n = adder' 0 n

adder' :: Int -> Int -> IO()
adder' sum 0 = do putStr "The total is "
                  putStrLn (show sum)
adder' sum n = do num <- getLine
                  adder' (sum + read num) (n-1)
