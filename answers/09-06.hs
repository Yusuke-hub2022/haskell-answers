
data Op = Add | Sub | Mul | Div | Pow -- Exercise a

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^" -- Exercise a

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y -- Exercise a

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

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

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
ops = [Add, Sub, Mul, Div, Pow] -- Exercise a

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

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

-- same as solutions'' in the book.
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', n == m]

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y =  y /= 0 && y /= 1 && x `mod` y == 0
valid Pow _ y = y > 0 -- Exercise a

-------------------------
-- Exercise a

-- To check the changes, see the comments in the code above.
-- 変更点は上のコードにコメントにしてあります。

-- > solutions' [1,2,3] 10
-- [1+(3^2)]

-------------------------
-- Exercise b

solutions'' :: [Int] -> Int -> [Result]
solutions'' ns n = map fst (filter (\ (r,d) -> d == smallest) ress')
                   where
                     ress     = [res | ns' <- choices ns, res <- results ns']
                     ress'    = [((e,m), abs (n-m)) | (e,m) <- ress]
                     ds       = [d | (r,d) <- ress']
                     smallest = foldr min (head ds) (tail ds)

-- > solutions'' [1,2,3] 11
-- [(1+(3^2),10)]

-------------------------
-- Exercise c

-- I decided to sort by amount of numbers in a expression
-- 式の項の少ない順に並べることにしました。
-- (2*1) < ((3-2)+1) = 2 < 3

-- reference 6.4
qsort :: Ord a => (b -> a) -> [b] -> [b]
qsort _ [] = []
qsort f (x:xs) = qsort f smaller ++ [x] ++ qsort f larger
                 where
                    smaller = [a | a <- xs, f a <= f x]
                    larger =  [b | b <- xs, f b > f x]

nums :: Result -> Int
nums (e,_) = length (values e)

solutions_c :: [Int] -> Int -> [Result]
solutions_c ns n = qsort nums (map fst (filter (\ (r,d) -> d == smallest) ress'))
                   where
                     ress     = [res | ns' <- choices ns, res <- results ns']
                     ress'    = [((e,m), abs (n-m)) | (e,m) <- ress]
                     ds       = [d | (r,d) <- ress']
                     smallest = foldr min (head ds) (tail ds)

-- > solutions_c [1,2,3,4] 2
-- [(2,2),(2^1,2),(3-1,2),(4/2,2),(4-2,2),((3-2)+1,2),(3-(2-1),2),...]





--main :: IO ()
--main = print (solutions'' [1,3,7,10,25,50] 765)




