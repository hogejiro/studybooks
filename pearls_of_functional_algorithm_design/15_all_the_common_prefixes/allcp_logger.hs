import Control.Monad.Writer

allcp xs = runWriter (untilW (done n) (step xs) (writer (([n], 0, 0, 1), [""])))
            where n = length xs

untilW b f w = do
    wr <- (b w)
    if wr then w else untilW b f (f w)

--done n (as, i, p, k) = k == n

first  (a, b, c, d) = a
fourth (a, b, c, d) = d
done n w = do
    (as, i, p, k) <- w
    tell ["iter " ++ show k ++ ":" ++ show (as, i, p)]
    return (k == n)

step xs w = do--(as, i, p, k)
    (as, i, p, k) <- w
    let q = as !! (k - i)
        r = p - (k - i)
        a = llcp xs (drop k xs)
        b = q + llcp (drop q xs) (drop (q + k) xs)
    if k >= i + p then do
        tell ["k:" ++ show k ++ ", i: " ++ show i ++ ", p: " ++ show p]
        return (snoc as         a, k, a, k + 1)
    else if q /= r then do
        tell ["eq! q:" ++ show q ++ ", r: " ++ show r]
        return (snoc as (min q r), i, p, k + 1)
    else do
        tell ["neq! q:" ++ show q ++ ", r: " ++ show r]
        return (snoc as         b, k, b, k + 1)

fst4 (a, b, c, d) = a
snoc xs x = xs ++ [x]

llcp xs [] = 0
llcp [] xs = 0
llcp (x : xs) (y : ys) = if x == y then 1 + llcp xs ys else 0

main = print $ (allcp "abacabacab")
