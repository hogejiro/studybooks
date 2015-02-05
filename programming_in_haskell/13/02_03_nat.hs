data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- exercise 2
-- Show that add n (Succ m) = Succ (add n m), by induction on n.
--
-- Base case:
--  add Zero (Succ m)
--  = { applying add }
--  Succ m
--  = { unapplying add }
--  Succ (add Zero m)
--
-- Inductive case:
--  add (Succ n) (Succ m)
--  = { applying add }
--  Succ (add n (Succ m))
--  = { induction hypothesis }
--  Succ (Succ (add n m))
--  = { unapplying add }
--  Succ (add (Succ n) m)

-- exercise 3
-- Using this property, together with add n Zero = n, show that addition is commutative, add n m = add m n, by induction on n.
--
-- Base case:
--  add Zero m
--  = { applying add }
--  m
--  = { applying "add n Zero = n" }
--  add m Zero
--
-- Inductive case:
--  add (Succ n) m
--  = { applying add }
--  Succ (add n m)
--  = { induction hypothesis }
--  Succ (add m n)
--  = { applying "add n (Succ m) = Succ (add n m)" }
--  add m (Succ n)
