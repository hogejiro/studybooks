data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs n (Leaf m)     = n == m
occurs n (Node l m r) = case compare n m of
                            LT -> occurs n l
                            EQ -> n == m
                            GT -> occurs n r

main = putStrLn $ show $ occurs 2 (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))
-- False
