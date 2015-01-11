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

-- (a) Define a natural grammar for such expressions.
-- expr ::= expr - nat | nat
-- nat ::= '0' | '1' | ...

-- (b) Translate this grammar into a parser expr :: Parser Int.

expr :: Parser Int
expr = expr >>>= \e -> symbol "-" >>>= const natural >>>= \n -> myreturn (e - n) +++ natural

-- (c) What is the problem with this parser?
-- Infinite loop happens. The reason why is because expr call expr recursively.

-- (d) Show how it can be fixed.
fix_expr :: Parser Int
fix_expr = natural >>>= \n -> many (symbol "-" >>>= const natural) >>>= \ns -> myreturn (foldl (-) n ns)
