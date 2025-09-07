type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x


instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = do st
                  st >>= g'
                  where g' = \x -> S (\s -> (g x, s))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))

   -- <*> :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = do stf
                    stx
                    S (\s ->
                       let (f,s') = app stf s in app (fmap f stx) s')

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> STb
   st >>= f = S (\s ->
      let (x,s') = app st s in app (f x) s')

-- Example

sta :: ST Int
sta = S (\s -> (s+1, s))

-- Functor
-- > app (fmap (*10) sta) 1
-- (20,1)

-- Applicative
-- > app (pure (*10) <*> sta) 1
-- (20,1)

-- Monad
-- > app (sta >>= \x -> S (\s -> (x*10, s))) 1
-- (20,1)

