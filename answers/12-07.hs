
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val x) = Val x
  fmap g (Add l r) = Add (fmap g l) (fmap g r)

-- > fmap (+10) (Var 1)
-- Var 11

-- > fmap (+10) (Add (Val 1) (Var 2))
-- Add (Val 1) (Var 12)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- (<+>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var g) <*> e = fmap g e

-- > pure (+10) <*> Var 1
-- Var 11

-- > pure (+10) <*> Add (Val 1) (Var 2)
-- Add (Val 1) (Var 12)

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var x) >>= g = g x
  (Val x) >>= _ = Val x
  (Add l r) >>= g = Add (l >>= g) (r >>= g)

-- > Var 1 >>= \x -> Val (x + 10)
-- Val 11

-- > Add (Val 1) (Var 2) >>= \x -> Val (x + 10)
-- Add (Val 1) (Val 12)

--  ">>=" function in this Expr type provides mutable bariables.
-- このExpr型において、>>= は変数の値の変更を可能にする。
