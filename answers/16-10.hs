
{-

instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b]
   xs >>= f = [y | x <- xs, y <- f x]

モナド則
   
   (1) return x >>= f = f x
   (2) mx >>= return = mx
   (3) (mx >>= f) >>= g = mx >>= (\x -> (fx >>= g))

(1)
return x >>= f = f x

     return x >>= f
        {returnを適用}
   = [x] >>= f
        {(>>=)を適用}
   = [y | x' <- [x], y <- f x']
        {リスト内包表記を[x]の最初の要素に適用}
   = f x ++ [y | x' <- [], y <- x']
        {リスト内包表記を適用}
   = f x ++ []
        {(++)を適用}
   = f x                                         □ 

(2)
mx >>= return = mx

     [x] >>= return
       {returnを逆適用}
   = return x >>= return
       {(1)より}
   = return x
       {returnを適用}
   = [x]                    □

(3)
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

     ([x] >>= f) >>= g
       {returnを逆適用}
   = (return x >>= f) >>= g
       {(1)より}
   = f x >>= g
       {無名関数を適用する形に変える}
   = (\x -> (f x >>= g)) x
       {(>>=)を逆適用}
   = return x >>= (\x -> (f x >>= g))
       {returnを適用}
   = [x] >>= (\x -> (f x >>= g))         □

-}
