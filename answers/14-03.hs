-- Researching

-- > fold (Just [1]) -- Using list as Monoid.
-- [1]

-- > foldMap (2:) (Just [1])
-- [2,1]

-- > foldMap (2:) Nothing
-- []

-- > foldr (++) [1] (Just [2])
-- [2,1]

-- > foldr (++) [1] Nothing
-- [1]

-- > foldl (++) [1] (Just [2])
-- [1,2]

-- > foldl (++) [1] Nothing
-- [1]

-- toList :: a -> [a] -- Using list as Applicable.
-- toList = \x -> [x]

-- > traverse toList (Just 1)
-- [Just 1]

-- > traverse toList Nothing
-- [Nothing]




-- Exercise 3

-- Not work on GHCi because of duplicate instance declarations
instance Foldable Maybe where
   -- Not (visible) method of Foldable class
   -- fold :: Monoid a => Maybe a -> a
   fold Nothing  = mempty
   fold (Just x) = x

   -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
   foldMap _ Nothing  = mempty
   foldMap f (Just x) = f x

   -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
   foldr f v Nothing  = v
   foldr f v (Just x) = f x v

   -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
   foldl f v Nothing  = v
   foldl f v (Just x) = f v x

-- Not work on GHCi because of duplicate instance declarations
instance Traversable Maybe where
   -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
   traverse _ Nothing = pure Nothing
   traverse g mx = pure g <*> mx


