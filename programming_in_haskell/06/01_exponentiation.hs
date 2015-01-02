(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1))

-- main = putStrLn $ show $ 2 ^^^ 3
-- 8

-- 2 ^^^ 3
-- = { applying ^^^ }
-- 2 * (2 ^^^ 2)
-- = { applying ^^^ }
-- 2 * (2 * (2 ^^^ 1))
-- = { applying ^^^ }
-- 2 * (2 * (2 * (2 ^^^ 0)))
-- = { applying ^^^ }
-- 2 * (2 * (2 * 1))
-- = { applying the third * }
-- 2 * (2 * 2)
-- = { applying the second * }
-- 2 * 4
-- = { applying * }
-- 8
