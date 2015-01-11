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

-- exercise 6
-- Extend the parser for arithmetic expressions to support subtraction and division, based upon the following extensions to the grammar
expr :: Parser Int
expr = term >>>= \t -> (symbol "+" >>>= const expr >>>= \e -> myreturn (t + e)) +++
                       (symbol "-" >>>= const expr >>>= \e -> myreturn (t - e)) +++
                       myreturn t

term :: Parser Int
term = factor >>>= \f -> (symbol "*" >>>= const term >>>= \t -> myreturn (f   *   t)) +++
                         (symbol "/" >>>= const term >>>= \t -> myreturn (f `div` t)) +++
                         myreturn f

-- exercise 7
-- Further extend the grammar and parser for arithmetic expressions to support exponentiation, which is assumed to associate to the right and have higher priority than multiplication and division, but lower priority than parentheses and numbers.
factor :: Parser Int
factor = unary >>>= \f -> (symbol "^" >>>= const term >>>= \t -> myreturn (f ^ t)) +++
                          myreturn f

unary :: Parser Int
unary = (symbol "(" >>>= const expr >>>= \e -> symbol ")" >>>= const (myreturn e)) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

-- exercise 1
-- The library file also defines a parser int :: Parser Int for an integer. Without looking at this definition, define int.
int :: Parser Int
int = (symbol "-" >>>= const natural >>>= \n -> myreturn (-n)) +++ natural

-- exercise 2
-- Define a parser comment :: Parser () for ordinary Haskell comments that begin with the symbol -- and extend to the end of the current line, which is represented by the control character ’\n’.

comment :: Parser ()
comment = string "--" >>>= const (many (sat (/= '\n'))) >>>= const (sat (== '\n')) >>>= const (myreturn ())


