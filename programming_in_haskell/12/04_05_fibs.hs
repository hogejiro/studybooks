-- exercise 4
-- Using a list comprehension, define an expression fibs :: [Integer] that gen- erates the infinite sequence of Fibonacci numbers
-- using the following simple procedure:
-- * the first two numbers are 0 and 1
-- * the next is the sum of the previous two
-- * return to the second step

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- exercise 5
-- Using fibs, define a function fib :: Int -> Integer that returns the nth Fibonnaci number (counting from zero), and an expression that calculates the first Fibonacci number greater than one thousand.

fib :: Int -> Integer
fib n = fibs !! n

main = print $ head $ dropWhile (< 1000) fibs
-- 1597
