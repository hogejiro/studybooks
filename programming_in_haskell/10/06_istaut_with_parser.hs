import Data.Char
import System.IO
import Control.Monad

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

instance Monad Parser where
    return v = P (\inp -> [(v, inp)])
    p >>= f = P (\inp -> case parse p inp of
                    []         -> []
                    [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
    mzero = P (\inp -> [])
    p `mplus` q = P (\inp -> case parse p inp of
                        []         -> parse q inp
                        [(v, out)] -> [(v, out)])

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\inp -> case inp of
            []     -> []
            (x:xs) -> [(x, xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

upper :: Parser Char
upper = sat isUpper

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      +++ nat

space :: Parser ()
space =  do many (sat isSpace)
            return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving Show

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

-- find first
find :: Eq a => a -> [(a, b)] -> b
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q)    = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
            where
                bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where
                vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

expr :: Parser Prop
expr = do t <- term
          do symbol "=>"
             e <- expr
             return (Imply t e)
             +++
             do symbol "=="
                e <- expr
                return (Equiv t e)
             +++ return t

term :: Parser Prop
term = do f <- factor
          do symbol "\\/"
             t <- term
             return (Or f t)
             +++
             do symbol "/\\"
                t <- term
                return (And f t)
             +++ return f

factor :: Parser Prop
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++
         do symbol "-"
            x <- upper
            return (Not (Var x))
         +++
         do x <- upper
            return (Var x)

eval_str :: String -> String
eval_str xs = case parse expr xs of
                [(n, [])] -> show (isTaut n)
                [(_, out)] -> "unused input:" ++ out
                _ -> "invalid input"

run :: IO ()
run = do putStrLn "Enter prop"
         xs <- getLine
         putStrLn ("result: [" ++ xs ++ "] is " ++ show (eval_str xs))
         run
