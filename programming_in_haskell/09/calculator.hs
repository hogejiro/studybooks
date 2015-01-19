import Data.Char
import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

type Parser a = String -> [(a, String)]

myreturn :: a -> Parser a
myreturn v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                [] -> []
                (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \x -> if p x then myreturn x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = myreturn []
string (x:xs) = char x >>>= const (string xs) >>>= const (myreturn (x:xs))

many :: Parser a -> Parser [a]
many p = many1 p +++ myreturn []

many1 :: Parser a -> Parser [a]
many1 p = p >>>= \v -> many p >>>= \vs -> myreturn (v:vs)

nat :: Parser Int
nat = (many1 digit) >>>= \xs -> myreturn (read xs)

space :: Parser ()
space = (many (sat isSpace)) >>>= \xs -> myreturn ()

token :: Parser a -> Parser a
token p = space >>>= const p >>>= \v -> space >>>= const (myreturn v)

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr = term >>>= \t -> (symbol "+" >>>= const expr >>>= \e -> myreturn (t + e)) +++
                       (symbol "-" >>>= const expr >>>= \e -> myreturn (t - e)) +++
                       myreturn t

term :: Parser Int
term = factor >>>= \f -> (symbol "*" >>>= const term >>>= \t -> myreturn (f   *   t)) +++
                         (symbol "/" >>>= const term >>>= \t -> myreturn (f `div` t)) +++
                         myreturn f

factor :: Parser Int
factor = (symbol "(" >>>= const expr >>>= \e -> symbol ")" >>>= const (myreturn e)) +++ natural

beep :: IO ()
beep = putStrLn "\BEL"

cls :: IO ()
cls = putStrLn "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---|",
       "| q | c | d | = |",
       "+---+---+---+---|",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---|",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---|",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---|",
       "| 0 | ( | ) | / |",
       "+---+---+---+---|"]

buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeat (4, 2) "             "
                writeat (4, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons
                then
                    process c xs
                else
                    do beep
                       calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])] -> calc (show n)
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
