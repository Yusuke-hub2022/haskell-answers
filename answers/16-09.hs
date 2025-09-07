{-

(12.2.3)
アプリカディブ則

   (1) pure id <*> x = x
   (2) pure (g x) = pure g <*> pure x
   (3) x <*> pure y = pure (\g -> g y) <*> x
   (4) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

(12.2.1)
instance Applicative Maybe where
   -- pure :: a -> Maybe a
   pure = Just

   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Nothing <*> _ = Nothing
   (Just g) <*> mx = fmap g mx

(12.1.1)
instance Functor Maybe where
   -- fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap _ Nothing = Nothing
   fmap g (Just x) = Just (g x)

-----------------
証明

(1)
pure id <*> x = x

   x = Nothing のとき

        pure id <*> Nothing
           {pureを適用}
      = (Just id) <*> Nothing
           {<*>を適用}
      = fmap id Nothing
           {fmapを適用}
      = Nothing

   x = Just xのとき

        pure id <*> Just x
           {pureを適用}
      =  Just id <*> Just x
           {<*>を適用}
      = fmap id (Just x)
           {fmapを適用}
      = Just (id x)
           {idを適用}
      = Just x                 □

(2)
pure (g x) = pure g <*> pure x

   x = Nothing のとき

        pure (g Nothing)
           {pureを適用}
      = Just (g Nothing)
           {fmapを逆適用}
      = fmap g (Just Nothing)
           {<*>を逆適用}
      = (Just g) <*> (Just Nothing)
           {pureを逆適用}
      = pure g <*> pure Nothing

   x = Just x のとき
        pure (g (Just x))
           {pureを適用}
      = Just (g (Just x))
           {fmapを逆適用}
      = fmap g (Just x)
           {<*>を逆適用}
      = Just g <*> Just (Just x)
           {pureを逆適用}
      = pure g <*> pure (Just x)     □

(3)
x <*> pure y = pure (\g -> g y) <*> x

   x = Nothing のとき

        Nothing <*> pure y
           {<*>を適用}
      = Nothing
           {fmapを逆適用}
      = fmap (\g -> g y) Nothing
           {<*>を逆適用}
      = pure (\g -> g y) <*> Nothing

   x = Just x のとき
        
        (Just x) <*> pure y
           {pureを適用}
      = (Just x) <*> (Just y)
           {<*>を適用}
      = fmap x (Just y)
           {fmapを適用}
      = Just (x y)
           {関数xを引数として受け取る形にする}
      = Just ((\g -> g y) x)
           {fmapを逆適用}
      = fmap (\g -> g y) (Just x)
           {<*>を逆適用}
      = Just (\g -> g y) <*> (Just x)
           {左辺にpureを逆適用}
      = pure (\g -> g y) <*> (Just x)           □

(4)
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
   
   x = Nothing のとき

        Nothing <*> (y <*> z)
           {左の<*>を適用}
      = Nothing
           {<*>を逆適用}
      = Nothing <*> z
           {<*>を逆適用}
      = (Nothing <*> y) <*> z
           {fmapを逆適用}
      = ((fmap (.) Nothing) <*> y) <*> z
           {<*>を逆適用}
      = ((Just (.) <*> Nothing) <*> y) <*> z
           {pureを逆適用}
      = ((pure (.) <*> Nothing) <*> y) <*> z
           {()を外す}
      = (pure (.) <*> Nothing <*> y ) <*> z

   x = Just x,
   y = Nothing のとき

        Just x <*> (Nothing <*> z)
           {左の<*>を適用}
      = fmap x (Nothing <*> z)
           {<*>を適用}
      = fmap x Nothing
           {fmapを適用}
      = Nothing
           {以下同様}

   x = Just x,
   y = Just y のとき

        Just x <*> (Just y <*> z)
           {1番目の<*>を適用}
      = fmap x (Just y <*> z)
           {<*>を適用}
      = fmap x (fmap y z)
           {(.)を逆適用}
      = ((fmap x) . (fmap y)) z
           {<*>を逆適用}
      = Just (.) <*> ((fmap x) (fmap y)) z
           {<*>を逆適用}
      = Just (.) <*> Just x <*> (fmap y) z
           {<*>を逆適用}
      = Just (.) <*> Just x <*> Just y <*> z
           {pureを逆適用}
      = pure (.) <*> Just x <*> Just y <*> z
           {()を補足}
      = (pure (.) <*> Just x <*> Just y) <*> z  □

-}

