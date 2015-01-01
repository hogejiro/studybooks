(&&&) :: Bool -> Bool -> Bool
b &&& c = if b == True then
            if c == True then True
            else False
          else False
