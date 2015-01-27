cls :: IO ()
cls = putStrLn "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "o" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                           (x - 1, y),                 (x + 1, y),
                           (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

{-
-- old version
births :: Board -> [Pos]
births b = [(x, y) | x <- [1..width],
                     y <- [1..height],
                     isEmpty b (x, y),
                     liveneighbs b (x, y) == 3]
-}

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- exercise 3
-- On some systems the game of life may flicker, due to the entire screen being cleared each generation. Modify the game to avoid such flicker by only redisplaying positions whose status changes.
new_life_loop :: Board -> Board -> IO ()
new_life_loop p b = do cls
                       new_showcells p b
                       wait 50000
                       new_life_loop b (nextgen b)

new_life :: Board -> IO ()
new_life b = new_life_loop [] b

new_showcells :: Board -> Board -> IO ()
new_showcells p b = seqn ([writeat cell " " | cell <- p] ++
                          [writeat cell "o" | cell <- b])

-- exercise 4
-- Produce an editor that allows the user to interactively create and modify the content of the board in the game of life.
life_edit :: Board -> IO Board
life_edit b = do cls
                 writeat (1, height + 2) "e: edit, s: start"
                 new_showcells [] b
                 putChar '\n'
                 putStr "> "
                 cmd <- getLine
                 case cmd of
                    "e" -> do putStr "x :"
                              x <- readLn :: IO Int
                              putStr "y :"
                              y <- readLn :: IO Int
                              life_edit ((x, y) : b)
                    "s" -> return b
                    otherwise -> life_edit b

new_life_edit :: IO ()
new_life_edit = do b <- life_edit []
                   new_life_loop [] b
