data Tree = Leaf Int | Node Tree Tree

balanced :: Tree -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

leaves :: Tree -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r
