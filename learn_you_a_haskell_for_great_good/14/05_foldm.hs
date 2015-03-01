import Control.Monad
import Control.Monad.Writer

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

binSmallsWithLog :: Int -> Int -> Writer [String] Int
binSmallsWithLog acc x
    | x > 9 = do
        tell [show x ++ " is too large."]
        return 0
    | otherwise = do
        tell ["add " ++ show x]
        return (acc + x)

main = do
    let xs = [2,  8, 3, 1]
    let ys = [2, 11, 3, 1]
    print $ foldM binSmalls 0 xs
    print $ foldM binSmalls 0 ys
    print $ foldM binSmalls 2 xs
    print $ foldM binSmalls 2 ys
    print $ runWriter $ foldM binSmallsWithLog 0 xs
    print $ runWriter $ foldM binSmallsWithLog 0 ys
    print $ runWriter $ foldM binSmallsWithLog 2 xs
    print $ runWriter $ foldM binSmallsWithLog 2 ys
