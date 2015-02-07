map f [] = []
map f (x : xs) = f x : map f xs
(f . g) x = f (g x)

-- map f (map g xs) = map (f . g) xs
--
-- Base case:
--  map f (map g [])
--  = { applying the second map }
--  map f []
--  = { applying map }
--  []
--  = { unapplying map }
--  map (f . g) []
--
-- Inductive case:
--  map f (map g (x : xs))
--  = { applying the second map }
--  map f (g x : map g xs)
--  = { applying the first map }
--  f (g x) : map f (map g xs)
--  = { induction hypothesis }
--  f (g x) : map (f . g) xs
--  = { unapplying . }
--  (f . g) x : map (f . g) xs
--  = { unapplying map }
--  map (f . g) (x : xs)
