main = do cs <- getContents
          print $ lastNLines 10 cs

lastNLines :: Int -> String -> String
lastNLines n = unlines . takeLast n . lines

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse
