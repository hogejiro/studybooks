import System.Environment

main = do
    args <- getArgs
    case args of
        []      -> error "input 2 args"
        [_]     -> error "input 2 args"
        (x:y:_) -> putStrLn ("1:" ++ show x ++ " 2:" ++ show y)
