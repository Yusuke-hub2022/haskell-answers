-- 08-05 --

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 08-06 --

e1 :: Expr
e1 = (Add (Val 1) (Add (Val 2) (Val 3)))

eval :: Expr -> Int
eval = folde id (+)

-- > eval e1
-- 6

size :: Expr -> Int
size = folde (\ _ -> 1) (+)

-- > size e1
-- 3
