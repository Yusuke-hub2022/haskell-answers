----------------------
-- a

-- Step 1
-- sum :: [Int] -> Int

-- Step 2
-- sum []     =
-- sum (x:xs) =

-- Step 3
-- sum []     = 0
-- sum (x:xs) =

-- Step 4
-- sum []     = 0
-- sum (x:xs) = x + (sum xs)

-- Step 5
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + (Main.sum xs)

-- Use "Main.sum" because Prelude already has "sum"

----------------------
-- b

-- Step 1
-- take :: Int -> [a] -> [a]

-- Step 2
-- take 0 (x:xs) = 
-- take n []     = 
-- take n (x:xs) = 

-- Step 3
-- take 0 (x:xs) = []
-- take n []     = []
-- take n (x:xs) = 

-- Step 4
-- take 0 (x:xs) = []
-- take n []     = []
-- take n (x:xs) = x : (take (n-1) xs)

-- Step 5
take :: Int -> [a] -> [a]
take 0 _ = []
take _ []     = []
take n (x:xs) = x : (Main.take (n-1) xs)

-- Use "Main.take" because Prelude already has "take"

----------------------
-- c

-- Step 1
-- last :: [a] -> a

-- Step 2
-- last (x:[]) =
-- last (x:xs) =

-- Step 3
-- last (x:[]) = x
-- last (x:xs) =

-- Step 4
-- last (x:[]) = x
-- last (x:xs) = last xs

-- Step 5
last :: [a] -> a
last (x:[]) = x
last (x:xs) = Main.last xs

-- Use "Main.last" because Prelude already has "last"


