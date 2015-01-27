data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

-- find first
find :: Eq a => a -> [(a, b)] -> b
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q)    = vars p ++ vars q

{-
bools :: Int -> [[Bool]]
bools n = map (map conv . make n . int2bin) [0..limit]
            where
                limit = (2 ^ n) - 1
                int2bin 0 = []
                int2bin n = n `mod` 2 : int2bin (n `div` 2)
                make n bs = take n (bs ++ repeat 0)
                conv 0 = False
                conv 1 = True
-}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
            where
                bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where
                vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 :: Prop
p5 = Imply (Var 'A') (Or (Var 'A') (Var 'B'))
p6 :: Prop
p6 = Equiv (Var 'A') (Or (Var 'A') (Var 'B'))
p7 :: Prop
p7 = Equiv (Var 'A') (Var 'A')

main = do putStrLn $ show $ isTaut p1
          putStrLn $ show $ isTaut p2
          putStrLn $ show $ isTaut p3
          putStrLn $ show $ isTaut p4
          putStrLn $ show $ isTaut p5
          putStrLn $ show $ isTaut p6
          putStrLn $ show $ isTaut p7
