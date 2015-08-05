tabStop = 8

main = getContents >>= \cs -> putStr $ expand cs

expand :: String -> String
expand cs = concatMap expandTab cs

expandTab :: Char -> String
expandTab '\t' = replicate tabStop ' '
expandTab c    = [c]
