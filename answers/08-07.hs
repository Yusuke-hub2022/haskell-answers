data MyMaybe a = MyNothing | MyJust a

instance Eq a => Eq (MyMaybe a) where
  MyNothing  == MyNothing  = True
  (MyJust x) == (MyJust y) = x == y
  _        == _        = False

-- > MyNothing == MyNothing
-- True

-- > (MyJust 1) == (MyJust 1)
-- True

-- > MyNothing == (MyJust 1)
-- False

-- > (MyJust 1) == (MyJust 2)
-- False

-- *Can not run in GHCi. Defined in 'GHC.Classes' already 
--instance Eq a => Eq [a] where
--   xs == ys = and [x == y | x <- xs, y <- ys]
