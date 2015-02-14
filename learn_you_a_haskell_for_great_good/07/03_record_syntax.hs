data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , height      :: Float
                     , phoneNumber :: String
                     , flavor      :: String } deriving (Show)

data Car = Car { company :: String
               , model   :: String
               , year    :: Int } deriving (Show)

main = do
        print $ car
        print $ company car
        print $ year car
            where car = Car {company="Ford", year=1967, model="Mustang"}
