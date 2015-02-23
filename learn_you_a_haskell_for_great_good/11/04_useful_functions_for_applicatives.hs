import Control.Applicative

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [[1, 2, 3], [4, 5, 6]]
  print $ sequenceA [(>4), (<10), odd] 7
