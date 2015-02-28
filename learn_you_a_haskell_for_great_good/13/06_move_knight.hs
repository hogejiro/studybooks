import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
               (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [[KnightPos]]
in3 start = do
  first  <- moveKnight start
  second <- moveKnight first
  third  <- moveKnight second
  return [first, second, third]

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` (map ((flip (!!)) 2) (in3 start))

safehead :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x

reachIn3 :: KnightPos -> KnightPos -> Maybe [KnightPos]
reachIn3 start end = safehead [pos | pos <- (in3 start), end == ((flip (!!)) 2) pos]

main = do
  print $ (6, 2) `reachIn3` (6, 1)
  print $ (6, 2) `reachIn3` (7, 3)
