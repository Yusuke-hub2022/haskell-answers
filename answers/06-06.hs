-- a
and :: [Bool] -> Bool
and [] = True
and (False:xs) = False
and (True:xs) = Main.and xs

-- Use "Main.and" on GHCi

-- b
concat :: [[a]] -> [a]
concat (xs:[]) = xs
concat (xs:xss) = xs ++ Main.concat xss

-- Use "Main.concat" on GHCi

-- c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n-1) x

-- Use "Main.replicate" on GHCi

-- d
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs Main.!! (n-1)

-- Use "Main.!!" on GHCi

-- e
elem :: Ord a => a -> [a] -> Bool
elem _ [] = False
elem x' (x:xs) | x' == x   = True
               | otherwise = Main.elem x' xs 

-- Use "Main.elem" on GHCi
