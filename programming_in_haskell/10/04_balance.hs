data Tree = Leaf Int | Node Tree Tree deriving Show

balance :: [Int] -> Tree
balance [n] = Leaf n
balance ns = Node (balance ls) (balance rs)
                where
                    (ls, rs) = halves ns

halves :: [Int] -> ([Int], [Int])
halves xs = splitAt (length xs `div` 2) xs

main = putStrLn $ show $ balance [1, 2, 3, 4, 5]
-- Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))
