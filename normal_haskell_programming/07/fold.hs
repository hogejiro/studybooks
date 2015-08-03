foldWidth :: Int
foldWidth = 60

main = do cs <- getContents
          putStr $ fold cs

fold :: String -> String
fold = unlines . concatMap foldLine . lines

foldLine :: String -> [String]
foldLine line = case splitAt foldWidth line of
                    (s, [])   -> [s]
                    (s, cont) -> s : foldLine cont
