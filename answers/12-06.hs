-- Not work on GHCi as duplicate declarations

-- from exercise 2
instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

-- from exercise 3
instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const

-- exercise 6

instance Monad ((->) a) where
  -- (>>=) :: (a -> b) -> (a -> b -> c) -> (a -> c)
  g >>= h = \x -> h (g x) x

