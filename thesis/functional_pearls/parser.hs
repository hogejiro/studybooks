import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c, cs)])

instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f = Parser (\cs -> concat [parse (f a) cs' |
                            (a, cs') <- parse p cs])

parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

-- MonadZero is obsoleted. Use MonadPlus, mzero
-- instance MonadZero Parser where
--     zero = Parser (\cs -> [])

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
--   '++' is not a (visible) method of MonadPlus class
--    p ++ q = Parser (\cs -> parse p cs ++ parse q cs)
    p `mplus` q = Parser (\cs -> case parse p cs of
                                    [] -> parse q cs
                                    (x:xs) -> [x])

-- (+++) :: Parser a -> Parser a -> Parser a
-- p +++ q = Parser (\cs -> case parse (p ++ q) cs of
--                    [] -> []
--                    (x:xs) -> [x])
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else mzero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do a <- p
             as <- many p
             return (a:as)

-- Parse repeated applications of a parser `p`,
-- separated by applications of a parser `sep` whose result values are thrown away
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

-- Parse repeated applications of a parser `p`,
-- separated by applications of a parser `op` whose result values is an operator that is assumed to associate to the left,
-- and which is used to combine the results from the p parsers.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do a <- p
                  rest a
                  where
                      rest a = (do f <- op
                                   b <- p
                                   rest (f a b))
                               +++ return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do a <- p
             space
             return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

expr :: Parser Int
expr = term   `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit +++ do symb "("
                      n <- expr
                      symb ")"
                      return n

digit :: Parser Int
digit = do x <- token (sat isDigit)
           return (ord x - ord '0')

addop :: Parser (Int -> Int -> Int)
addop = do symb "+"
           return (+)
        +++
        do symb "-"
           return (-)

mulop :: Parser (Int -> Int -> Int)
mulop = do symb "*"
           return (*)
        +++
        do symb "/"
           return (div)
