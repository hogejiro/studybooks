data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatTree :: a -> Tree a
repeatTree x = t where t = Node t x t

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _    = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node l c r) = Node (takeTree (n - 1) l) c (takeTree (n - 1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree

main = print $ replicateTree 3 "Apple"
-- Node (Node (Node Leaf "Apple" Leaf) "Apple" (Node Leaf "Apple" Leaf)) "Apple" (Node (Node Leaf "Apple" Leaf) "Apple" (Node Leaf "Apple" Leaf))
