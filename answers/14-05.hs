import Data.Foldable

-- Definitions

data Tree a = Leaf a | Node (Tree a) (Tree a)
     deriving Show

instance Foldable Tree where
   -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
   foldMap f (Leaf x) = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- Exercise 5

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])

-- > filterF odd [1,2,3,4]
-- [1,3]

-- > filterF odd tree
-- [1,3]
