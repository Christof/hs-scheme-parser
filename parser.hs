{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad
import           Control.Monad.Except
import           Data.Array
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | Vector (Array Int LispVal)
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | Float Double
  | Character Char
  | String String
  | Bool Bool

data LispError
  = NumArgs Integer
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialForm String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = "Error: " ++ message

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseExplicit :: String -> Parser Char -> (String -> Integer) -> Parser LispVal
parseExplicit prefix digitParser conversion = do
  _ <- try $ string prefix
  x <- many1 digitParser
  (return . Number . conversion) x

parseExplicitDecimal :: Parser LispVal
parseExplicitDecimal = parseExplicit "#d" digit read

hexToDigit :: (Eq a, Num a) => String -> a
hexToDigit x = fst $ readHex x !! 0

parseHex :: Parser LispVal
parseHex = parseExplicit "#h" hexDigit hexToDigit

octToDigit :: (Eq a, Num a) => String -> a
octToDigit x = fst $ readOct x !! 0

parseOct :: Parser LispVal
parseOct = parseExplicit "#o" octDigit octToDigit

binToDigit :: [Char] -> Integer
binToDigit = binToDigit' 0

binToDigit' :: Integer -> [Char] -> Integer
binToDigit' digint "" = digint
binToDigit' digint (x:xs) =
  let old =
        2 * digint +
        (if x == '0'
           then 0
           else 1)
   in binToDigit' old xs

binDigit = oneOf "10"

parseBin :: Parser LispVal
parseBin = parseExplicit "#b" binDigit binToDigit

parseNumber :: Parser LispVal
parseNumber =
  parseDecimal <|> parseExplicitDecimal <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  (return . Float . fst . head . readFloat) (x ++ "." ++ y)

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $
    case x of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      _   -> x -- for \\ and "

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf "\\\""
  _ <- char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do
  _ <- try $ string "#\\"
  x <-
    try (string "newline" <|> string "space") <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $
    Character $
    case x of
      "space"    -> ' '
      "newline"  -> '\n'
      _otherwise -> (x !! 0)

parseListElements :: Parser LispVal
parseListElements = liftM List $ sepBy parseExpr spaces

parseDottedListElements :: Parser LispVal
parseDottedListElements = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
  _ <- string "("
  x <- try parseListElements <|> parseDottedListElements
  _ <- char ')'
  return x

parseVectorElements :: Parser LispVal
parseVectorElements = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  x <- parseVectorElements
  _ <- char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> try parseFloat <|> try parseNumber <|> try parseBool <|>
  try parseChar <|>
  parseString <|>
  parseQuoted <|>
  parseQuasiQuoted <|>
  parseUnQuote <|>
  parseVector <|>
  parseListOrDottedList

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector array) = "(" ++ unwordsList (elems array) ++ ")"

instance Show LispVal where
  show = showVal

numericBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _op [] = throwError $ NumArgs 2 []
numericBinop _op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _          = Bool False

isCharacter :: [LispVal] -> LispVal
isCharacter [Character _] = Bool True
isCharacter _             = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False

isFloat :: [LispVal] -> LispVal
isFloat [Float _] = Bool True
isFloat _         = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _        = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _        = Bool False

isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _        = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom s] = String s
symbolToString _        = String ""

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [String s] = Atom s
stringToSymbol _          = Atom ""

boolBinop ::
     (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString notString  = throwError $ TypeMismatch "string" notString

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_xs)]         = return x
car [DottedList (x:_xs) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_x:xs)]         = return $ List xs
cdr [DottedList [_] x]     = return x
cdr [DottedList (_x:xs) y] = return $ DottedList xs y
cdr [badArg]               = throwError $ TypeMismatch "pair" badArg
cdr badArgList             = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]             = return $ List [x]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqvList ::
     ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eqvFunc [x1, x2] of
        Left _err        -> False
        Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [list1@(List _arg1), list2@(List _arg2)] = eqvList eqv [list1, list2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [list1@(List _arg1), list2@(List _arg2)] = eqvList equal [list1, list2]
equal [(DottedList xs x), (DottedList ys y)] =
  equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitveEquals <-
    liftM or $
    mapM
      (unpackEquals arg1 arg2)
      [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $
    Bool $
    (primitveEquals ||
     let (Bool x) = eqvEquals
      in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("string?", return . isString)
  , ("char?", return . isCharacter)
  , ("number?", return . isNumber)
  , ("real?", return . isFloat)
  , ("symbol?", return . isSymbol)
  , ("boolean?", return . isBoolean)
  , ("list?", return . isList)
  , ("symbol->string", return . symbolToString)
  , ("string->symbol", return . stringToSymbol)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

cond :: [LispVal] -> ThrowsError LispVal
cond ((List (Atom "else":value:[])):[]) = eval value
cond ((List (condition:value:[])):alts) = do
  result <- eval condition
  boolResult <- unpackBool result
  if boolResult
    then eval value
    else cond alts
cond ((List a):_) = throwError $ NumArgs 2 a
cond (a:_) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Vector _) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool True -> eval conseq
    Bool False -> eval alt
    _otherwise ->
      throwError $ BadSpecialForm "If predicate must return a bool" pred
eval (List ((Atom "cond"):alts)) = cond alts
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval val@(List _) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaluated <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaluated
