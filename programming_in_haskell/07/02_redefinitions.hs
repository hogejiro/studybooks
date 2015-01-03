myall :: [Bool] -> Bool
myall = foldr (&&) True

-- main = putStrLn $ show $ myall [True, True, False]
-- False

myany :: [Bool] -> Bool
myany = foldr (||) False

-- main = putStrLn $ show $ myany [True, True, False]
-- True

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ []     = []
mytakeWhile p (x:xs) | p x       = x : mytakeWhile p xs
                     | otherwise = []

-- main = putStrLn $ show $ mytakeWhile even [2, 4, 1, 3, 5, 6]
-- [2,4]

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile _ []     = []
mydropWhile p (x:xs) | p x       = mydropWhile p xs
                     | otherwise = x:xs

-- main = putStrLn $ show $ mydropWhile even [2, 4, 1, 3, 5, 6]
-- [1,3,5,6]
