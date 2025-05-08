luhnDouble :: Int -> Int
luhnDouble n = if m > 9 then m - 9 else m
               where m = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if sum `mod` 10 == 0 then True else False
               where sum = (luhnDouble a) + b + (luhnDouble c) + d
