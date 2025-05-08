unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

-- chop8
chop8 = unfold null (take 8) (drop 8)

-- map f
map f = unfold null (f . head) (tail)

-- > Main.map (+1) [1,2,3]
-- [2,3,4]

-- iterate f
iterate f = unfold (\ _ -> False) f f

-- > take 5 (Main.iterate (+1) 0)
-- [1,2,3,4,5]
