import Control.Monad

count w = length . filter (== w) . words

count_file w = (>>= print) .
               liftM (length . filter (== w) . words) .
               readFile

type Kleisli m a b = a -> m b

-- readFile' :: Kleisli IO String String
-- print' :: Kleisli IO a ()

(>>>) :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do b <- f a
                 g b

arr :: Monad m => (a -> b) -> Kleisli m a b
arr f = return . f

count_file' w = readFile >>>
                arr words >>> arr (filter (== w)) >>> arr length >>>
                print
