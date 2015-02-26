import Control.Monad

main = do
  print $ [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
  print $ guard (5 > 2) >> (return "cool" :: [String])
  print $ guard (1 > 2) >> (return "cool" :: [String])
