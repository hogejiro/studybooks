myand :: [Bool] -> Bool
myand []     = True
myand (b:bs) = b && myand bs

-- main = putStrLn $ show $ myand [True, False, True]
-- False

myconcat :: [[a]] -> [a]
myconcat []     = []
myconcat (xs:xss) = xs ++ myconcat xss

-- main = putStrLn $ show $ myconcat [[1], [2, 3], [4, 5]]
-- [1,2,3,4,5]

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x

-- main = putStrLn $ show $ myreplicate 5 True
-- [True,True,True,True,True]

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

-- main = putStrLn $ show $ [1, 2, 3, 4, 5] !!! 3
-- 4

myelem :: Eq a => a -> [a] -> Bool
myelem x []     = False
myelem x (y:ys) | x == y    = True
                | otherwise = myelem x ys

-- main = putStrLn $ show $ myelem 5 [1, 2, 3, 4, 5]
-- True
