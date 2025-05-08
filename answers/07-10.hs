
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f h (x:xs) = f x : altMap h f xs

luhnDouble :: Int -> Int
luhnDouble n = if m > 9 then m - 9 else m
               where m = n * 2

luhn :: [Int] -> Bool
luhn xs = if n `mod` 10 == 0 then True else False
          where n = sum (altMap luhnDouble id (reverse xs))

-- luhn [2,2,2]
-- True
