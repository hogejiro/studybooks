take 0 _ = []
take (n + 1) [] = []
take (n + 1) (x : xs) = x : take n xs

drop 0 xs = xs
drop (n + 1) [] = []
drop (n + 1) (x : xs) = drop n xs

-- take n xs ++ drop n xs = xs
--
-- Base case: n = 0
--  take 0 xs ++ drop 0 xs
--  = { applying take }
--  [] ++ drop 0 xs
--  = { applying drop }
--  [] ++ xs
--  = { applying ++ }
--  xs
--
-- Base case: xs = []
--  take n [] ++ drop n []
--  = { applying take }
--  [] ++ drop n []
--  = { applying drop }
--  [] ++ []
--  = { applying ++ }
--  []
--
-- Inductive case:
--  take (n + 1) (x : xs) ++ drop (n + 1) (x : xs)
--  = { applying take }
--  (x : take n xs) ++ drop (n + 1) (x : xs)
--  = { applying drop }
--  (x : take n xs) ++ drop n xs
--  = { applying distributivity ++ }
--  x : (take n xs ++ drop n xs)
--  = { induction hypothesis }
--  x : xs
