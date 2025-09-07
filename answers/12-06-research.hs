
g :: Num a => a -> a
g = (\x -> x + 1)

h :: Num a => a -> a -> a
h = (\x y -> x - y)

myf = g >>= h

-- > myf 10
-- 1
