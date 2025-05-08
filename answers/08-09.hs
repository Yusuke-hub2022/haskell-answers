
data Expr = Val Int | Add Expr Expr | Mult Expr Expr -- Change

type Cont = [Op]

data Op = EVA Expr | EVM Expr | ADD Int | MULT Int -- Change

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVA y : c) -- Change
eval (Mult x y) c = eval x (EVM y : c) -- Change

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVA y : c) n = eval y (ADD n : c) -- Change
exec (EVM y : c) n = eval y (MULT n : c) -- Add
exec (ADD n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m) -- Add

value :: Expr -> Int
value e = eval e []

-- > value (Mult (Val 2) (Add (Val 2) (Val 3)))
-- 10
