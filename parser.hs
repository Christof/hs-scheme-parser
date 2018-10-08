{-# LANGUAGE ExistentialQuantification #-}

import           Control.Monad
import           Control.Monad.Except
import           Data.Array
import           Data.IORef
import           Numeric
import           System.Environment
import           System.IO
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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params  :: [String]
         , vararg  :: (Maybe String)
         , body    :: [LispVal]
         , closure :: Env }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

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

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left error)  = throwError error
liftThrows (Right value) = return value

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = _env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing  -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

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

strLength :: [LispVal] -> ThrowsError LispVal
strLength [String s]  = Right $ Number $ fromIntegral $ length s
strLength [notString] = throwError $ TypeMismatch "string" notString
strLength badArgList  = throwError $ NumArgs 1 badArgList

strRef :: [LispVal] -> ThrowsError LispVal
strRef [String s, Number i]
  | length s < i' + 1 = throwError $ Default "Out of bound error"
  | otherwise = Right $ Character $ s !! i'
  where
    i' = fromIntegral i
strRef [notString, Number _] = throwError $ TypeMismatch "string" notString
strRef [String _, notNumber] = throwError $ TypeMismatch "number" notNumber
strRef badArgList = throwError $ NumArgs 2 badArgList

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

instance Eq LispVal where
  x == y =
    case equal [x, y] of
      Right (Bool True) -> True
      _                 -> False

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
  , ("string-length", strLength)
  , ("string-ref", strRef)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>=
  (flip bindVars $
   map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
         evalBody
  where
    num = toInteger . length
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ reaminingArgs)]
        Nothing -> return env
    evalBody env = liftM last $ mapM (eval env) body
    reaminingArgs = drop (length params) args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
appyProc (func : args) = apply func args


cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env ((List (Atom "else":value:[])):[]) = eval env value
cond env ((List (condition:value:[])):alts) = do
  result <- eval env condition
  boolResult <- liftThrows $ unpackBool result
  if boolResult
    then eval env value
    else cond env alts
cond _env ((List a):_) = throwError $ NumArgs 2 a
cond _env (a:_) = throwError $ NumArgs 2 [a]
cond _env _ = throwError $ Default "Not viable alternative in cond"

caseExpression ::
     Env -> LispVal -> [LispVal] -> LispVal -> IOThrowsError LispVal
caseExpression env key clauses form =
  case head clauses of
    List (Atom "else":exprs) -> mapM (eval env) exprs >>= return . last
    List ((List datums):exprs) -> do
      result <- eval env key
      equality <- mapM (\x -> (liftThrows . eqv) [result, x]) datums
      if Bool True `elem` equality
        then mapM (eval env) exprs >>= return . last
        else eval env $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form

makeFunc ::
     Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _env val@(String _) = return val
eval _env val@(Character _) = return val
eval _env val@(Number _) = return val
eval _env val@(Float _) = return val
eval _env val@(Bool _) = return val
eval _env val@(Vector _) = return val
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define":List (Atom var:params):body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (Atom id) = getVar env id
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    _otherwise ->
      throwError $ BadSpecialForm "If predicate must return a bool" pred
eval env (List ((Atom "cond"):alts)) = cond env alts
eval _env (List [Atom "quote", val]) = return val
eval env form@(List (Atom "case":key:clauses)) =
  if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else caseExpression env key clauses form
eval env (List (function:args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _env val@(List _) = return val
eval _env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser "lisp" input of
    Left error  -> throwError $ Parser error
    Right value -> return value

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

loopUntil :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
loopUntil pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> loopUntil pred prompt action

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
  loopUntil (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0          -> runRepl
    1          -> runOne $ args !! 0
    _otherwise -> putStrLn "Program takes only 0 or 1 argument"
