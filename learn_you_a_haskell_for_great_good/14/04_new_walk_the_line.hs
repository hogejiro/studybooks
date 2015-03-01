type Bird = Int
type Pole = (Bird, Bird)

landLeft :: Int -> Pole -> Either String Pole
landLeft x (l, r)
    | abs (l + x - r) < 4 = Right (l + x, r)
    | otherwise           = Left ("drop down @ landLeft " ++ show x ++ " from " ++ show (l, r))

landRight :: Int -> Pole -> Either String Pole
landRight x (l, r)
    | abs (r + x - l) < 4 = Right (l, r + x)
    | otherwise           = Left ("drop down @ landRight " ++ show x ++ " from " ++ show (l, r))

routine :: Either String Pole
routine = do
  start  <- return (0, 0)
  first  <- landLeft 1 start
  second <- landRight 2 first
  third  <- landRight 2 second
  landRight 1 third

banana :: Pole -> Either String Pole
banana pos = Left ("drop down @ banana : " ++ show pos)

main = print $ routine
