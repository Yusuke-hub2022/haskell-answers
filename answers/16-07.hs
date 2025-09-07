{-
-- 12.1.2 関手則
(1) fmap id = id
(2) fmap (g . h) = fmap g . fmap h

-- 12.1.1
instance Functor Maybe where
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Noting = Nothing
fmap g (Just x) = Just (g x)

----------------------
証明

(1) fmap id = id

   Nothing:

        fmap id Nothing
           {fmapを適用}     
      = Nothing
           {idを逆適用}
      = id Nothing
      
   Just x:

        fmap id (Just x)
           {fmapを適用}
      = Just (id x)
           {idを適用}
      = Just x
           {idを逆適用}
      = id (Just x)           □


(2) fmap (g . h) = fmap g . fmap h

   Nothing:

        fmap (g . h) Nothing
           {fmapを適用}
      = Noghing
           {fmap g を逆適用}
      = fmap g Nothing
           {fmap h を逆適用}
      = fmap g (fmap h Noghing)
           {. を逆適用}
      = fmap g . fmap h Nothing

   Just x:

        fmap (g . h) (Just x)
           {fmapを適用}
      = Just (g . h x)
           {. を適用}
      = Just (g (h x))
           {fmap g を逆適用}
      = fmap g (Just (h x))
           {fmap h を逆適用}
      = fmap g (fmap h (Just x))
           {. を逆適用}
      = fmap g . fmap h (Just x)   □
-}
