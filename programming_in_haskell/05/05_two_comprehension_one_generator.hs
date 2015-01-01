-- one comprehension, two generators
-- list = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

-- two comprehensions
list = concat [[(x, y) | y <- [4, 5, 6], x <- [1, 2, 3]]]

-- main = putStrLn $ show $ list
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5),(1,6),(2,6),(3,6)]
