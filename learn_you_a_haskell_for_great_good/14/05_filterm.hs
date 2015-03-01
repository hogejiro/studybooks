import Control.Monad
import Control.Monad.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [False, True]) xs

main = do
    let xs = [9, 1, 5, 2, 10, 3]
    print $ fst $ runWriter $ filterM keepSmall xs
    mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall xs
    print $ powerset [1..3]
