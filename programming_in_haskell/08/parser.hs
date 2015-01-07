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

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

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

ident :: Parser String
ident = lower >>>= \x -> (many alphanum) >>>= \xs -> myreturn (x:xs)

nat :: Parser Int
nat = (many1 digit) >>>= \xs -> myreturn (read xs)

space :: Parser ()
space = (many (sat isSpace)) >>>= \xs -> myreturn ()

token :: Parser a -> Parser a
token p = space >>>= const p >>>= \v -> space >>>= const (myreturn v)

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

natural_list :: Parser [Int]
natural_list = symbol "[" >>>= const natural >>>= \n -> (many (symbol "," >>>= const natural)) >>>= \ns -> symbol "]" >>>= const (myreturn (n:ns))

