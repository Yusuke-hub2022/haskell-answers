
-- 9.2

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- 9.3

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- 9.4

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- 9.5

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

e :: Expr
e = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))) -- (1+50)*(25-10)

-- 9.6

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- 9.7

-- Update in 9.8
-- main :: IO ()
-- main = print (solutions [1,3,7,10,25,50] 765)

-- 9.8

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                   lx <- results ls,
                   ry <- results rs,
                   res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', n == m]

-- main :: IO ()
-- main = print (solutions' [1,3,7,10,25,50] 765)

-- 9.9

-- valid :: Op -> Int -> Int -> Bool
-- valid Add x y = x <= y
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x <= y
-- valid Div x y =  y /= 1 && x `mod` y == 0




-------------------------------------------
-- Exercise 4

-- Turn off when Exercise 5
--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = x > y
--valid Mul _ _ = True
--valid Div x y = x `mod` y == 0


es :: [Expr]
es = concat (map exprs (choices [1,3,7,10,25,50]))

vs :: [Int]
vs = concat (map eval es)

main :: IO ()
main = print ((length es, length vs))

-- $ ghc -O2 09-04_05.hs
-- $ ./09-04_05
-- (33665406,4672540)

-------------------------------------------
-- Exercise 5

-- Turn off when Exercise 4
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True -- Change
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0 -- Change

-- $ ghc -O2 09-04_05.hs
-- $ ./09-04_05
-- (33665406,10839369)

