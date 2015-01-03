-- [f x | x <- xs, p x]
map f $ filter p xs

-- xs = [1..5]
-- p = even
-- f x = x ^ 2
-- main = putStrLn $ show $ map f $ filter p xs
-- [4,16]
