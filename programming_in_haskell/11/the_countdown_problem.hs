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

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- Brute force solution
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

-- exercise 3
-- What effect would generalising the function split to also return pairs containing the empty list have on the behaviour of solutions?
-- infinite loop.

-- exercise 4
-- Using choices, exprs, and eval, verify that there are 33,665,406 possible expressions over the numbers 1, 3, 7, 10, 25, 50, and that only 4, 672, 540 of these expressions evaluate successfully.
-- main = putStrLn $ show $ length [e | ns <- choices [1,3,7,10,25,50], e <- exprs ns]
-- 33665406
-- main = putStrLn $ show $ length [e | ns <- choices [1,3,7,10,25,50], e <- exprs ns, eval e /= []]
-- 4672540

-- exercise 5
-- Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369 if the numeric domain is generalised to arbitrary integers.
-- valid Sub x y = True
-- valid Mul x y = y /= 0 && x `mod` y == 0
-- main = putStrLn $ show $ length [e | ns <- choices [1,3,7,10,25,50], e <- exprs ns, eval e /= []]
-- 10839369

