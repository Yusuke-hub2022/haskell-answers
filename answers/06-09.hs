----------------------
-- a

-- Step 1
-- mysum :: [Int] -> Int

-- Step 2
-- mysum []     =
-- mysum (x:xs) =

-- Step 3
-- mysum []     = 0
-- mysum (x:xs) =

-- Step 4
-- mysum []     = 0
-- mysum (x:xs) = x + (mysum xs)

-- Step 5
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + (mysum xs)

----------------------
-- b

-- Step 1
-- mytake :: Int -> [a] -> [a]

-- Step 2
-- mytake 0 (x:xs) = 
-- mytake n []     = 
-- mytake n (x:xs) = 

-- Step 3
-- mytake 0 (x:xs) = []
-- mytake n []     = []
-- mytake n (x:xs) = 

-- Step 4
-- mytake 0 (x:xs) = []
-- mytake n []     = []
-- mytake n (x:xs) = x : (mytake (n-1) xs)

-- Step 5
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ []     = []
mytake n (x:xs) = x : (mytake (n-1) xs)

----------------------
-- c

-- Step 1
-- mylast :: [a] -> a

-- Step 2
-- mylast (x:[]) =
-- mylast (x:xs) =

-- Step 3
-- mylast (x:[]) = x
-- mylast (x:xs) =

-- Step 4
-- mylast (x:[]) = x
-- mylast (x:xs) = mylast xs

-- Step 5
mylast :: [a] -> a
mylast (x:[]) = x
mylast (x:xs) = mylast xs

