-- a
myand :: [Bool] -> Bool
myand [] = True
myand (False:xs) = False
myand (True:xs) = myand xs

-- b
myconcat :: [[a]] -> [a]
myconcat (xs:[]) = xs
myconcat (xs:xss) = xs ++ concat xss

-- c
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x

-- d
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

-- e
myelem :: Ord a => a -> [a] -> Bool
myelem _ [] = False
myelem x' (x:xs) | x' == x   = True
               | otherwise = myelem x' xs 
