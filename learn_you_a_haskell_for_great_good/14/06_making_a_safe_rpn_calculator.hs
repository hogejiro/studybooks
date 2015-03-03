import Data.List
import Control.Monad

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _         -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*"  = return $ (y * x)  : ys
foldingFunction (x:y:ys) "+"  = return $ (y + x)  : ys
foldingFunction (x:y:ys) "-"  = return $ (y - x)  : ys
foldingFunction (x:y:ys) "/"  = return $ (y / x)  : ys
foldingFunction (x:y:ys) "^"  = return $ (y ** x) : ys
foldingFunction (x:xs)  "ln"  = return $ log x : xs
foldingFunction xs      "sum" = return $ [sum xs]
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

main = do
  print $ solveRPN "10 10 10 10 sum 4 /"
  print $ solveRPN "10 2 ^"
  print $ solveRPN "1 2 * 4 + 5 *"
  print $ solveRPN "1 2 * 4"
  print $ solveRPN "1 8 wharglbllargh"
