myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _ <- [1..n]]

-- main = putStrLn (show (myreplicate 3 True))
-- [True,True,True]
