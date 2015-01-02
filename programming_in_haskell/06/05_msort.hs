halve :: [a] -> ([a], [a])
halve xs = splitAt half xs
    where
        half = (length xs) `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort ms) (msort ns)
    where
        (ms, ns) = halve xs

-- main = putStrLn $ show $ msort [6, 5, 2, 4, 3, 1]
-- [1,2,3,4,5,6]
