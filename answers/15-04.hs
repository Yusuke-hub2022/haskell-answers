
fibs :: [Integer]
fibs = [0,1] ++ [x + y | (x,y) <- zip fibs (tail fibs)]

-- > take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
