unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Integer] -> [[Integer]]
chop8 = unfold null (take 8) (drop 8)

-- main = putStrLn $ show $ chop8 [1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0]
-- [[1,0,1,1,0,0,1,1],[0,1,0,1,1,1,0,0]]

mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f = unfold null (f . head) tail

-- main = putStrLn $ show $ mymap even [1..5]
-- [False,True,False,True,False]

myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (const False) id f
-- main = putStrLn $ show $ take 5 (myiterate (*2) 2)
-- [2,4,8,16,32]
