reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- lemma: reverse (xs ++ [x]) = x : reverse xs

-- reverse (reverse xs) = xs
--
-- Base case:
--  reverse (reverse [])
--  = { applying reverse }
--  reverse []
--  = { applying reverse}
--  []
--
-- Inductive case:
--  reverse (reverse (x : xs))
--  = { applying the second reverse }
--  reverse (reverse xs ++ [x]))
--  = { applying lemma }
--  reverse (x : reverse xs)
--  = { applying the first reverse }
--  reverse (reverse (reverse xs) ++ [x])
--  = { induction hypothesis }
--  reverse (xs ++ [x])
--  = { unapplying reverse }
--  reverse (x : xs)
--
-- singleton lists, distributivity, associativity (add) are more generally-useful than above lemma.
