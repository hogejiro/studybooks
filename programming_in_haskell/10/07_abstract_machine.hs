data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]
data Op   = EVALA Expr | ADD Int | EVALM Expr | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVALA y : c)
eval (Mul x y) c = eval x (EVALM y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVALA y : c) n = eval y (ADD n : c)
exec (EVALM y : c) n = eval y (MUL n : c)
exec (ADD  m : c) n = exec c (m + n)
exec (MUL  m : c) n = exec c (m * n)

value :: Expr -> Int
value e = eval e []

main = print $ value (Add (Mul (Val 2) (Val 3)) (Val 4))
-- (2 * 3) + 4
-- 10
