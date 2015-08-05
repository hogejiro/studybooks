main = getContents >>= \cs -> print $ firstNLines 10 cs

firstNLines :: Int -> String -> String
firstNLines n = unlines . take n . lines
