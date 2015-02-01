data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = (concat (map (interleave x) (perms xs)))

choices :: [a] -> [[a]]
choices xs = (concat (map perms (subs xs)))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- exercise 1
-- Redefine the combinatorial function choices using a list comprehension rather than the library functions concat and map.
choices_comprehension :: [a] -> [[a]]
choices_comprehension xs = [xs'' | xs' <- subs xs, xs'' <- perms xs']

-- exercise 2
-- Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from another, without using the combinatorial functions perms and subs.
removeOne :: Eq a => a -> [a] -> [a]
removeOne x [] = []
removeOne x (y:ys) | x == y    = ys
                   | otherwise = y : removeOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _  []     = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeOne x ys)

split :: [a] -> [([a], [a])]
split []  = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]
