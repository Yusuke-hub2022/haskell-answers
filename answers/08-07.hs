data Maybe a = Nothing | Just a

instance Eq a => Eq (Main.Maybe a) where
  Main.Nothing  == Main.Nothing  = True
  (Main.Just x) == (Main.Just y) = x == y
  _        == _        = False

-- Main.Nothing == Main.Nothing
-- True

-- (Main.Just 1) == (Main.Just 1)
-- True

-- Main.Nothing == (Main.Just 1)
-- False

-- (Main.Just 1) == (Main.Just 2)
-- False

-- Duplicate declarations error
instance Eq a => Eq [a] where
  xs == ys = and [x == y | x <- xs, y <- ys]
