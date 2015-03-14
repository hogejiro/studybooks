import System.Environment

main = do
    args <- getArgs
    case args of
        []  -> error "please input args (but it's to be ignored...)"
        [_] -> do
                putStrLn "please input your name"
                line <- getLine
                putStrLn $ "Your name is: " ++ line
