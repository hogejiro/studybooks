import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        let c = a `mod` b
        tell [show a ++ " mod " ++ show b ++ " = " ++ show c]
        gcd' b c

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    tell (toDiffList [show x])

main = do
  print $ runWriter multWithLog
  print $ runWriter $ gcd' 8 3
  print $ fromDiffList (toDiffList [1..4] `mappend` toDiffList [1..3])
  mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 100

