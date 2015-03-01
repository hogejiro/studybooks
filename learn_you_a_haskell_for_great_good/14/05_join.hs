import Control.Monad

main = do
  print $ (join (Right (Right 9)) :: Either String Int)
  print $ (join (Right (Left "error")) :: Either String Int)
  print $ join [[1, 2, 3], [4, 5, 6]]
