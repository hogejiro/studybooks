mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\x xs -> f x:xs) []

-- main = putStrLn $ show $ mymap even [1..5]
-- [False,True,False,True,False]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\x xs -> if p x then x:xs else xs) []

-- main = putStrLn $ show $ myfilter even [1..5]
-- [2,4]
