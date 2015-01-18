import Data.Char

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


