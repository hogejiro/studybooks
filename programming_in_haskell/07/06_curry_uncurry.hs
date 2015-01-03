mycurry :: ((a, b) -> c) -> (a -> b -> c)
mycurry f = \x y -> f (x, y)

-- add (x, y) = x + y
-- main = putStrLn $ show $ mycurry add 1 2
-- 3

myuncurry :: (a -> b -> c) -> ((a, b) -> c)
myuncurry f = \(x, y) -> f x y

-- main = putStrLn $ show $ myuncurry map ((^2),  [1..3])
-- [1,4,9]
