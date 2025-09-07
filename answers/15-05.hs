
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeattree :: a -> Tree a
repeattree x = tree where tree = Node tree x tree

taketree :: Int -> Tree a -> Tree a
taketree 0 _    = Leaf
taketree _ Leaf = Leaf
taketree n (Node l x r) = Node (taketree (n-1) l) x (taketree (n-1) r)

-- > taketree 2 (repeattree 1)
-- Node (Node Leaf 1 Leaf) 1 (Node Leaf 1 Leaf)

reptree :: Int -> a -> Tree a
reptree 0 _ = Leaf
reptree n x = Node (reptree (n-1) x) x (reptree (n-1) x)

-- > reptree 2 1
-- Node (Node Leaf 1 Leaf) 1 (Node Leaf 1 Leaf)
