import Control.Monad.State
import System.Random

type Stack = [Int]

pop :: State Stack Int
pop = state $ (\(x:xs) -> (x, xs))

push :: Int -> State Stack ()
push x = state $ (\xs -> ((), (x:xs)))

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

main = do
  print $ runState stackManip [5, 8, 2, 1]
  gen <- getStdGen
  print $ fst $ runState threeCoins gen
