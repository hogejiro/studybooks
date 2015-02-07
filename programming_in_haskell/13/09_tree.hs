data Tree = Leaf Int | Node Tree Tree

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

-- nodes t + 1 = leaves t
--
-- Base case:
--  nodes (Leaf n) + 1
--  = { applying nodes }
--  0 + 1
--  = { applying + }
--  1
--  = { unapplying leaves }
--  leaves (Leaf n)
--
-- Indutive case:
--  nodes (Node l r) + 1
--  = { applying nodes }
--  1 + nodes l + nodes r + 1
--  = { arithmetic }
--  (nodes l + 1) + (nodes r + 1)
--  = { indutive hypothesis }
--  leaves l + leaves r
--  = { unappyying leaves }
--  leaves (Node l r)
