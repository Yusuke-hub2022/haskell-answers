
adder :: Int -> IO ()
adder n =  do ms <- sequence [getLine | _ <- [1..n]]
              putStr "The total is "
              putStrLn (show (sum (map read ms)))
