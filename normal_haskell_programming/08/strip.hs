main = do line <- getLine
          putStr $ strip line

lstrip :: String -> String
lstrip = dropWhile (== ' ')

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = rstrip . lstrip
