import qualified Data.List as DL

main = do cs <- getContents
          putStrLn $ sort cs

sort :: String -> String
sort = unlines . map DL.sort . lines
