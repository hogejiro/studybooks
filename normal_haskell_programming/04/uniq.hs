import Data.List

main = do cs <- getContents
          putStrLn $ uniq cs

uniq :: String -> String
uniq = unlines . map head . group . lines
