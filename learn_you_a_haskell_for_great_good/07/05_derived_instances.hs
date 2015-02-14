data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

main = do print $ succ Monday
          print $ pred Saturday
          print $ [Thursday .. Sunday]
          print $ [minBound :: Day .. maxBound]
