{-
------------------------
1

pure id <*> x = x

- Left side

> pure id <*> Just 1
Just 1  -- Equals right side of the expression

- Type x

> :type Just 1
just 1 :: Num a => Maybe a

------------------------
2

pure (g x) = pure g <*> pure x

- Left side

> pure ((+ 1) 10)
11

- Right side

> pure (+ 1) <*> pure 10
11

- Type g

> :type (+ 1)
(+ 1) :: Num a => a -> a

- Type x

> :type 10
10 :: Num a => a

------------------------
3

x <*> pure y = pure (\g -> g y) <*> x

- Left side

> Just (+ 1) <*> pure 10
Just 11

- Right side

> pure (\g -> g 10) <*> Just (+ 1)
Just 11

- Type x

> :type Just (+ 1)
Just (+ 1) :: Num a => Maybe (a -> a)

- Type y

> :type 10
10 :: Num a => a


------------------------
4

x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

- Left side

> Just (+ 1) <*> (Just (+ 2) <*> Just 10)
Just 13

- Right side

> (pure (.) <*> Just (+ 1) <*> Just (+ 2)) <*> Just 10
Just 13

- Type x

> :type Just (+ 1)
Just (+ 1) :: Num a => Maybe (a -> a)

- Type y

> :type Just (+ 2)
Just (+ 2) :: Num a => Maybe (a -> a)

- Type z

> :type Just 10
Just 10 :: Num a => Maybe a

-}
