import System.Environment

main = do
    args <- getArgs
    case args of
        (x:y:_) -> do
                    mapM_ operate ["+", "-", "*", "/"]
                        where operate op = putStrLn $ x ++ " " ++ op ++ " " ++ y ++ " = " ++ show (opFromStr op x y)
                              opFromStr op x y = op' x' y'
                                where op' =
                                        case op of
                                            "+" -> (+)
                                            "-" -> (-)
                                            "*" -> (*)
                                            "/" -> div
                                            otherwise -> error "invalid operator"
                                      x' = (read x) :: Int
                                      y' = (read y) :: Int
        _     -> error "input 2 args"
