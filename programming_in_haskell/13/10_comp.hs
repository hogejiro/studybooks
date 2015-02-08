data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- comp' e c = comp e ++ c
--
-- Base case:
--  comp' (Val n) c
--  = { applying comp' }
--  comp (Val n) ++ c
--  = { applying comp }
--  [PUSH n] ++ c
--  = { applying ++ }
--  (PUSH n) : c
--
-- Inductive case:
--  comp' (Add x y) c
--  = { applying comp' }
--  comp (Add x y) ++ c
--  = { applying comp }
--  comp x ++ comp y ++ [ADD] ++ c
--  = { applying distributivity ++ }
--  comp x ++ (comp y ++ [ADD] ++ c)
--  = { induction hypothesis x }
--  comp' x (comp y ++ [ADD] ++ c)
--  = { applying distributivity ++ }
--  comp' x (comp y ++ ([ADD] ++ c))
--  = { applying ++ }
--  comp' x (comp y ++ (ADD : c)
--  = { induction hypothesis y }
--  comp' x (comp' y (ADD : c))
