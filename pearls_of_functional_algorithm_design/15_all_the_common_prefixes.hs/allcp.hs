tails [x] = [[x]]
tails xs = xs : tails (tail xs)

llcp xs [] = 0
llcp [] xs = 0
llcp (x:xs) (y:ys) = if x == y then 1 + llcp xs ys else 0

allcp xs = map (llcp xs) (tails xs)

main = print $ (allcp "abacabacab")
