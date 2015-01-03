dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- main = putStrLn $ show $ dec2int [2, 3, 4, 5]
-- 2345
