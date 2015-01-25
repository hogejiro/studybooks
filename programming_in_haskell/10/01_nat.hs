data Nat = Zero | Succ Nat deriving Show

add Zero m     = m
add (Succ n) m = Succ (add n m)

mult Zero        m = Zero
mult (Succ n)    m = add m (mult n m)

-- main = putStrLn $ show $ mult (Succ (Succ (Succ Zero))) (Succ (Succ (Succ (Succ Zero))))
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))
