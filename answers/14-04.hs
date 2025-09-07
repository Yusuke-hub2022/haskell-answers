import Data.Foldable

-- Definitions

dec :: Int -> Maybe Int
dec n = if n > 0 then Just(n-1) else Nothing

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

tree :: Tree [Int]
tree = Node (Node Leaf [2] Leaf) [1] (Node Leaf [3] Leaf)

-- Exercise 4

instance Functor Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ Leaf = Leaf
   fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
   -- fold :: Monoid a => Tree a -> a
   fold Leaf = mempty
   fold (Node l x r) = x `mappend` fold l `mappend` fold r

   -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
   foldMap _ Leaf = mempty
   foldMap f (Node l x r) = f x `mappend` foldMap f l `mappend` foldMap f r

   -- foldr :: (a -> b -> b) -> b -> Tree a -> b
   foldr f v Leaf = v
   foldr f v (Node l x r) = f x (foldr f (foldr f v r) l)

   -- foldl :: (a -> b -> a) -> a -> Tree b -> a
   foldl f v Leaf = v
   foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

instance Traversable Tree where
   -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
   traverse _ Leaf = pure Leaf
   traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

-- > fold tree
-- [1,2,3]

-- > foldMap (map (+10)) tree
-- [11,12,13]

-- > foldr (++) [] tree
-- [1,2,3]

-- > foldl (++) [] tree
-- [1,2,3]

-- > traverse dec (Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf))
-- Just (Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf))

-- > traverse dec (Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf))
-- Nothing
