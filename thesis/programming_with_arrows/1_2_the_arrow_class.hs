import Control.Monad

class Arrow r where
    arr :: (a -> b) -> r a b
    (>>>) :: r a b -> r b c -> r a c

instance Arrow (->) where
    arr   = id
    (>>>) = flip (.)

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
    Kleisli f >>> Kleisli g = Kleisli (\a -> do b <- f a
                                                g b)
    arr f = Kleisli (return . f)

count w = Kleisli readFile >>>
          arr words >>> arr (filter (== w)) >>> arr length >>>
          Kleisli print

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)

delay x = SF (x:)
