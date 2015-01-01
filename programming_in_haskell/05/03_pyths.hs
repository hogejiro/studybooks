pyths   :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- main = putStrLn $ show $ pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
