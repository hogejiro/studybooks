import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Numeric
import Data.Char hiding (isNumber)
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

data LispVal = Atom String
            | Vector (Array Int LispVal)
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | Float Double
            | String String
            | Bool Bool
            | Character Char
            | Ratio Rational
            | Complex (Complex Double)

showVal :: LispVal -> String
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

escapedChars :: Parser Char
escapedChars = do c <- char '\\' >> oneOf "\\\"nrt"
                  return $ case c of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _   -> c

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> escapedChars)
                 char '"'
                 return $ String x

-- variable
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    return $ Atom (first:rest)

parseComplex :: Parser LispVal
parseComplex = do
    realPart <- try parseFloat <|> parseNumber
    spaces >> char '+' >> spaces
    imagPart <- try parseFloat <|> parseNumber
    char 'i'
    return $ Complex ((toDouble realPart) :+ (toDouble imagPart))
        where toDouble (Float  f) = f
              toDouble (Number n) = fromIntegral n

parseFloat :: Parser LispVal
parseFloat = do
    integerPart <- many1 digit
    char '.'
    fractionalPart <- many1 digit
    return $ Float (fst . head $ readFloat (integerPart ++ "." ++ fractionalPart))

parseRational :: Parser LispVal
parseRational = do
    numerator <- many1 digit
    spaces >> char '/' >> spaces
    denominator <- many1 digit
    return $ Ratio ((read numerator) % (read denominator))

parseNumber :: Parser LispVal
parseNumber = do
    num <- parseInt <|> parseDecimal <|> parseHex <|> parseOct <|> parseBin
    return num

parseInt :: Parser LispVal
parseInt = do
    int <- many1 digit
    return $ (Number . read) int

parseDecimal :: Parser LispVal
parseDecimal = do
    try $ string "#d"
    int <- many1 digit
    return $ (Number . read) int

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    hex <- many1 hexDigit
    return $ Number (hex2dig hex)

hex2dig x = fst $ head $ readHex x

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    oct <- many1 octDigit
    return $ Number (oct2dig oct)

oct2dig x = fst $ head $ readOct x

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    bin <- many1 (oneOf "01")
    return $ Number (bin2dig bin)

bin2dig bs = toInteger $ foldr (\x y -> x + y * 2) 0 (map c2i bs)
            where c2i n = ord n - 48

parseBool :: Parser LispVal
parseBool = do
    char '#'
    b <- oneOf "tf"
    return $ case b of
        't' -> Bool True
        'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    char <- try (string "newline" <|> string "space") <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case char of
        "space"   -> ' '
        "newline" -> '\n'
        _         -> (char !! 0)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    quoted <- parseExpr
    return $ List [Atom "quasiquote", quoted]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    unquoted <- parseExpr
    return $ List [Atom "unquote", unquoted]

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseList :: Parser LispVal
parseList = do
    char '(' >> spaces
    head <- parseExpr `sepEndBy` spaces1
    do char '.' >> spaces1
       tail <- parseExpr
       spaces >> char ')'
       return $ DottedList head tail
       <|> do spaces >> char ')'
              return $ List head

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseQuoted
        <|> try parseQuasiQuoted
        <|> try parseUnQuote
        <|> try (do string "#("
                    vector <- parseVector
                    char ')'
                    return vector)
        <|> try parseList

eval :: LispVal -> ThrowsError LispVal
eval val @ (String _) = return val
eval val @ (Number _) = return val
eval val @ (Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
        Bool False -> eval alt
        Bool True  -> eval conseq
        _          -> throwError $ TypeMismatch "bool" pred
eval (List ((Atom "cond"):cs)) =
    do b <- (liftM (take 1 . dropWhile f) $ mapM condClause cs) >>= cdr
       car [b] >>= eval
        where
            condClause (List [p, b]) = do q <- eval p
                                          case q of
                                            Bool _ -> return $ List [q, b]
                                            _      -> throwError $ TypeMismatch "bool" q
            condClause v             = throwError $ TypeMismatch "(pred body)" v
            f                        = \(List [p, b]) -> case p of
                                                            (Bool False) -> True
                                                            _            -> False

eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",              numericBinop (+)),
              ("-",              numericBinop (-)),
              ("*",              numericBinop (*)),
              ("/",              numericBinop div),
              ("mod",            numericBinop mod),
              ("quotient",       numericBinop quot),
              ("remainder",      numericBinop rem),
              ("=",              numBoolBinop (==)),
              ("/=",             numBoolBinop (/=)),
              ("<",              numBoolBinop (<)),
              ("<=",             numBoolBinop (<=)),
              (">",              numBoolBinop (>)),
              (">=",             numBoolBinop (>=)),
              ("&&",             boolBoolBinop (&&)),
              ("||",             boolBoolBinop (||)),
              ("string=?",       strBoolBinop (==)),
              ("string<?",       strBoolBinop (<)),
              ("string<=?",      strBoolBinop (<=)),
              ("string>?",       strBoolBinop (>)),
              ("string>=?",      strBoolBinop (>=)),
              ("car",            car),
              ("cdr",            cdr),
              ("cons",           cons),
              ("eq?",            eqv),
              ("eqv?",           eqv),
              ("equal?",         equal),
              ("atom?",          unaryOp isAtom),
              ("string?",        unaryOp isString),
              ("number?",        unaryOp isNumber),
              ("bool?",          unaryOp isBool),
              ("list?",          unaryOp isList),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal @ [_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

isAtom :: LispVal -> LispVal
isAtom (Atom _) = Bool True
isAtom _        = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _        = Bool False

isList :: LispVal -> LispVal
isList (List _)         = Bool True
isList (DottedList _ _) = Bool True
isList _                = Bool False

symbol2string :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _        = String ""

string2symbol :: LispVal -> LispVal
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

showError :: LispError -> String
showError (NumArgs        expected found)   = "Expected" ++ show expected ++ "args: found values " ++ unwordsList found
showError (TypeMismatch   expected found)   = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser         parseErr)         = "Parse error at " ++ show parseErr
showError (BadSpecialForm message  form)    = message ++ ": " ++ show form
showError (NotFunction    message  func)    = message ++ ": " ++ show func
showError (UnboundVar     message  varname) = message ++ ": " ++ varname

instance Show LispError where show = showError

instance Error LispError where
    noMsg  = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError:: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool   arg1), (Bool   arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom   arg1), (Atom   arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all equalPair $ zip arg1 arg2)
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals  <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqFuncPair :: ([LispVal] -> ThrowsError LispVal) -> (LispVal, LispVal) -> Bool
eqFuncPair eqFunc (x1, x2)  = case eqFunc [x1, x2] of
    Left  err        -> False
    Right (Bool val) -> val

eqvPair :: (LispVal, LispVal) -> Bool
eqvPair = eqFuncPair eqv

equalPair :: (LispVal, LispVal) -> Bool
equalPair = eqFuncPair equal

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
