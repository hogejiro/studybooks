solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y * x) : ys
        foldingFunction (x:y:ys) "+" = (y + x) : ys
        foldingFunction (x:y:ys) "-" = (y - x) : ys
        foldingFunction (x:y:ys) "/" = (y / x) : ys
        foldingFunction (x:y:ys) "^" = (y ** x) : ys
        foldingFunction (x:xs)  "ln" = log x : xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs

main = do
  print $ solveRPN "10 10 10 10 sum 4 /"
  print $ solveRPN "10 2 ^"