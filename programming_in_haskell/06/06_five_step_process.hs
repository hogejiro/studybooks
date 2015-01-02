-- sum
-- Step 1: define the type
-- sum :: [Int] -> Int

-- Step 2: enumerate the cases
-- sum []   =
-- sum x:xs =

-- Step 3: define the simple cases
-- sum []     = 0
-- sum (x:xs) =

-- Step 4: define the other cases
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- Step 5: generalise and simplify
-- sum :: Num a => [a] -> Int
-- sum = foldr (+) 0

-- take
-- Step 1: define the type
-- take :: Int -> [a] -> [a]

-- Step 2: enumerate the cases
-- take 0 []     =
-- take 0 (x:xs) =
-- take n []     =
-- take n (x:xs) =

-- Step 3: define the simple cases
-- take 0 []     = []
-- take 0 (x:xs) = []
-- take n []     =
-- take n (x:xs) =

-- Step 4: define the other cases
-- take 0 []     = []
-- take 0 (x:xs) = []
-- take n []     = []
-- take n (x:xs) = x : take (n - 1) xs

-- Step 5: generalise and simplify
-- take :: Int -> [a] -> [a]
-- take 0 _      = []
-- take n []     = []
-- take n (x:xs) = x : take (n - 1) xs

-- last
-- Step 1: define the type
-- last :: [a] -> a

-- Step 2: enumerate the cases
-- last (x:xs) | null xs   =
--             | otherwise =

-- Step 3: define the simple cases
-- last (x:xs) | null xs   = x
--             | otherwise =

-- Step 4: define the other cases
-- last (x:xs) | null xs   = x
--             | otherwise = last xs

-- Step 5: generalise and simplify
-- last :: [a] -> a
-- last [x]    = x
-- last (_:xs) = last xs
