data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap g (Leaf x) = Leaf (g x)
   fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-

-- 12.1.2 関手則
(1) fmap id = id
(2) fmap (g . h) = fmap g . fmap h

----------------------
証明

(1) fmap id = id
   
   基底部：
      
        fmap id (Leaf x)
           {fmapを適用}
      = Leaf (id x)
           {idを適用}
      = Leaf x
           {idを逆適用}
      = id (Leaf x)

   再帰部：

        fmap id (Node l r)
           {fmapを適用}
      = Node (fmap id l) (fmap id r)
           {仮定より}
      = Node (id l) (id r)
           {idを適用}
      = Node l r
           {idを逆適用}
      = id (Node l r)                   □


(2) fmap (g . h) = fmap g . fmap h

   基底部：

        fmap (g . h) (Leaf x)
           {fmapを適用}
      = Leaf (g . h x)
           {.を適用}
      = Leaf (g (h x))
           {fmap gを逆適用}
      = fmap g (Leaf (h x))
           {fmap hを逆適用}
      = fmap g (fmap h (Leaf x))
           {.を逆適用}
      = fmap g . fmap h (Leaf x)

   再帰部：

        fmap (g . h) (Node l r)
           {fmapを適用}
      = Node (fmap (g . h) l) (fmap (g . h) r)
           {仮定より}
      = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
           {fmapを逆適用}
      = (fmap g . fmap h) (Node l r)                     □

-}
