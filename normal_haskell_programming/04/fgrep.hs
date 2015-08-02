import System.Environment
import Data.List

main = do args <- getArgs
          cs <- getContents
          putStr $ fgrep (head args) cs

fgrep :: String -> String -> String
fgrep pattern = unlines . filter match . lines
    where
        match :: String -> Bool
        match = any (pattern `isPrefixOf`) . tails
