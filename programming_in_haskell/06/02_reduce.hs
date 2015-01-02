mylength :: [a] -> Int
mylength []   = 0
mylength (_:xs) = 1 + mylength xs

-- main = putStrLn $ show $ mylength [1, 2, 3]
-- 3

-- mylength [1, 2, 3]
-- = { applying mylength }
-- 1 + mylength [2, 3]
-- = { applying mylength }
-- 1 + (1 + mylength [3])
-- = { applying mylength }
-- 1 + (1 + (1 + mylength []))
-- = { applying mylength }
-- 1 + (1 + (1 + 0))
-- = { applying the third + }
-- 1 + (1 + 1)
-- = { applying the second + }
-- 1 + 2
-- = { applying + }
-- 3

mydrop :: Int -> [a] -> [a]
mydrop 0 xs   = xs
mydrop n []   = []
mydrop n (_:xs) = mydrop (n - 1) xs

-- main = putStrLn $ show $ mydrop 3 [1, 2, 3, 4, 5]
-- [4,5]

-- mydrop 3 [1, 2, 3, 4, 5]
-- = { applying mydrop }
-- mydrop 2 [2, 3, 4, 5]
-- = { applying mydrop }
-- mydrop 1 [3, 4, 5]
-- = { applying mydrop }
-- mydrop 0 [4, 5]
-- = { applying mydrop }
-- [4, 5]

myinit :: [a] -> [a]
myinit [_]   = []
myinit (x:xs) = x : myinit xs

-- main = putStrLn $ show $ myinit [1, 2, 3]
-- [1,2]

-- myinit [1, 2, 3]
-- = { applying myinit }
-- 1 : myinit [2, 3]
-- = { applying myinit }
-- 1 : (2 : myinit [3])
-- = { applying myinit }
-- 1 : (2 : [])
-- = { applying the second : }
-- 1 : [2]
-- = { applying : }
-- [1, 2]
