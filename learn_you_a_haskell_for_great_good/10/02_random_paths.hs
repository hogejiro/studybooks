import System.Random
import System.Environment
import System.IO
import Control.Monad

main = do
  args <- getArgs
  case args of
    []             -> putStrLn "nothing inputs"
    (num:argList)  -> do let length = (read num :: Int)
                         paths <- (getPaths length)
                         if elem "-o" argList
                            then mapM_ putStrLn paths
                            else return ()
                         forM_ paths (\line -> appendFile "random_paths.txt" (line ++ "\n"))

getPaths :: Int -> IO [String]
getPaths length = do
  gen <- getStdGen
  let rand3MultipleNumbers = take (3 * length) $ randomRs (0, 100) gen :: [Int]
      paths = map (\x -> show x) rand3MultipleNumbers
  newStdGen
  return paths
