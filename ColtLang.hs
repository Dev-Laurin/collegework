-- ColtLang.hs  
-- Laurin Fisher 

--Started By: 
-- Glenn G. Chappell
-- Started 6 Apr 2015
-- Updated 17 Apr 2015
--
-- For CS 331 Spring 2014
-- Parsing & Evaluation for Colt Programming Language
-- (Colt = Cs331's Other Lisp Thing)
-- Not a complete program

module ColtLang where

import Text.ParserCombinators.Parsec


-- ***** Type Definitions *****


-- Type Value
-- Holds a Lisp value.
data Value =
      NoValue
    | Err String
    | Pair Value Value
    | Null
    | Symbol String
    | Number Integer
    | Str String
    | Boolean Bool
    | Func (DefList -> [Value] -> (DefList, Value))


-- showVal
-- Show function for Value.
showVal :: Value -> String
showVal NoValue = ""
showVal (Err msg) = "*** ERROR: " ++ msg
showVal (Pair x y) = "(" ++ showVal x ++ showValEndList y where
    showValEndList (Pair x y) = " " ++ showVal x ++ showValEndList y
    showValEndList Null = ")"
    showValEndList z = " . " ++ showVal z ++ ")"
showVal Null = "()"
showVal (Symbol name) = name
showVal (Number n) = show n
showVal (Str s) = "\"" ++ s ++ "\""
showVal (Boolean b) = if b then "#t" else "#f"
showVal (Func _) = "<procedure>"

instance Show Value where show = showVal


-- quoteIt
-- Utility function for making quoted Values: x -> (quote x).
quoteIt :: Value -> Value
quoteIt x = Pair (Symbol "quote") (Pair x Null)


-- isNoValue
-- Utility function for checking for NoValue.
isNoValue :: Value -> Bool
isNoValue NoValue = True
isNoValue _ = False


-- Type Deflist
-- Holds a list of symbol definitions.
-- A DefList is a Haskell list. Each item is a pair: (String, Value).
-- The Value is the value of the symbol whose name is given by the String.
-- Symbols with unlisted names are taken to be undefined. Earlier
-- definitions override later ones.
type DefList = [(String, Value)]


-- ***** Parsing *****


-- listToVal
-- Convert a Haskell list to a Value.
listToVal :: [Value] -> Value
listToVal [] = Null
listToVal (x:xs) = Pair x (listToVal xs)


-- Parsing Functions

-- doParse
-- Given a String, attempt to parse it as a Colt expression. Return
-- value is return of parser combinator: (Left String) for error
-- message, (Right Value) for correctly parsed value.
doParse :: String -> Either ParseError Value
doParse input = parse parseExpr "Colt" input


-- special
-- Parse a special character that is legal at the start of a symbol.
special :: Parser Char
special = oneOf "!$%&|*+-/:<=>?@^_~"


-- skipSpace
-- Skip one or more whitespace characters.
skipSpace :: Parser ()
skipSpace = skipMany1 space


-- parseSymbolNum
-- Parse an atom containing only letters, digits, special characters,
-- and '#", not beginnning with '#'. Determine whether it is a symbol or
-- a number, and return the appropriate Value.
parseSymbolNum :: Parser Value
parseSymbolNum = do
   first <- letter <|> digit <|> special
   rest <- many (letter <|> digit <|> special <|> char '#')
   let str = (first:rest)
   if isNumber str then do
       let num = readNumber str
       return (Number num)
   else
       return (Symbol str)
       where
       isNumber ('+':c:cs) = all isDigit (c:cs)
       isNumber ('-':c:cs) = all isDigit (c:cs)
       isNumber (c:cs) = all isDigit (c:cs)
       isNumber _ = False
       isDigit c = c >= '0' && c <= '9'
       readNumber ('+':c:cs) = read (c:cs)  -- No '+' in Haskell Integer
       readNumber str = read str


-- parseStr
-- Parse a double-quoted string, e.g., "abc" - no backslash escapes.
parseStr :: Parser Value
parseStr = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    return (Str s)


-- parsePound
-- Parse a literal beginning with a pound sign, e.g., #t. Only handles
-- Boolean values.
parsePound :: Parser Value
parsePound = do
    char '#'
    c <- oneOf "tf"
    return $ Boolean (c == 't')


-- parseQuoted
-- Parse quoted value, like 'x or '(a b c).
parseQuoted :: Parser Value
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ quoteIt x


-- parseListoid
-- Parse a list or list plus dot-value at end, e.g., (val val val) or
-- (val val . val).
parseListoid :: Parser Value
parseListoid = do
    char '('
    myVal <- parseListoid'
    skipMany space
    char ')'
    return myVal
    where
    parseListoid' :: Parser Value
    parseListoid' = do
        skipMany space
        first <- (try parseExpr <|> return NoValue)
        if isNoValue first then return Null
        else do
            tail <- (try getDotExpr <|> return NoValue)
            if isNoValue tail then do
                rest <- parseListoid'
                return $ Pair first rest
            else return $ Pair first tail
    getDotExpr :: Parser Value
    getDotExpr = do
        skipMany space
        char '.'
        skipMany space
        val <- parseExpr
        return val


-- parseExpr
-- Parse a Lisp expression; start symbol in our grammar.
parseExpr :: Parser Value
parseExpr = do
    skipMany space
    parseSymbolNum <|>
      parseStr <|>
      parsePound <|>
      parseQuoted <|>
      parseListoid


-- ***** Evaluation *****


-- err
-- Given function name & message, returns Value representing evaluation
-- error.
err :: String -> String -> Value
err fn msg = Err ("evaluation\n" ++ fn ++ ": " ++ msg)


-- valToList
-- Convert a Value (must be Pair or Null) to a Haskell list.
valToList :: Value -> [Value]
valToList Null = []
valToList (Pair x y) = x:valToList y
valToList _ = [err "valToList (internal func)"
                   "Value is not Pair or Null; cannot convert to list"]


-- evalWithDef
-- Given Value to evaluate, and context (existing defines), returns
-- pair: result of evaluation and new context.
evalWithDef :: DefList -> Value -> (DefList, Value)
evalWithDef d (Pair h pl) = callFunc (evalExpr d h) d (valToList pl) where
    callFunc (Func f) d ps = f d ps
    callFunc (Err e) d _ = (d, Err e)
    callFunc _ d _ = (d, err "eval" "No procedure call")
evalWithDef d (Symbol name) = (d, symbolVal $ lookup name d) where
    symbolVal (Just v) = v
    symbolVal Nothing = err "eval" ("Symbol `" ++ name ++ "' not defined")
evalWithDef d Null = (d, err "eval" "No procedure call")
evalWithDef d v = (d, v)
  -- Above line for: NoValue, Err, Number, Str, Boolean, Func


-- evalExpr
-- Given Value to evaluate, and context (existing defines), returns
-- result of evaluation.
evalExpr :: DefList -> Value -> Value
evalExpr d v = v' where
    (_, v') = evalWithDef d v


-- normFunc
-- Make a Func value out of a "normal" function.
normFunc :: ([Value] -> Value) -> Value
normFunc nf = Func f where
    f d vs = (d, nf $ map (evalExpr d) vs)


-- Implementation of special primitive procedures.
-- See predefs (below) for symbol-primitive bindings.

sfuncDefine :: DefList -> [Value] -> (DefList, Value)
sfuncDefine d [Symbol s, v] = (d', NoValue) where
    d' = (s, v'):d
    v' = evalExpr d' v
sfuncDefine d _ = (d, err "define" "Must pass symbol, value")

sfuncEval :: DefList -> [Value] -> (DefList, Value)
sfuncEval d [v] = evalWithDef d' v' where
    (d', v') = evalWithDef d v
sfuncEval d _ = (d, err "eval" "Must pass single value")

sfuncQuote :: DefList -> [Value] -> (DefList, Value)
sfuncQuote d [v] = (d, v)
sfuncQuote d _ = (d, err "quote" "Must pass single value")

sfuncLambda :: DefList -> [Value] -> (DefList, Value)
sfuncLambda d [Symbol name, v] = (d, Func f) where
    f dd ps = (dd, evalExpr d' v) where
        paramdef = (name, listToVal $ map (evalExpr dd) ps)
        d' = paramdef:d
sfuncLambda d [sl, v]
  | isSymbolList sl  = (d, Func f) where
    f dd ps
      | length ps == length ss  = (dd, evalExpr d' v) where
        ss = map symbolName $ valToList sl
        symbolName (Symbol name) = name
        paramdefs = zip ss (map (evalExpr dd) ps)
        d' = paramdefs ++ d
    f dd _ = (dd, err "(lambda func)" "Must pass parameter(s), expression")
    isSymbolList Null = True
    isSymbolList (Pair (Symbol _) rest) = isSymbolList rest
    isSymbolList _ = False
sfuncLambda d _ = (d, err "lambda" "Must pass parameter(s), expression")


-- Implementation of "normal" primitive procedures:
-- All parameters eval'd, no global bindings created or modified.
-- See predefs (below) for symbol-primitive bindings.

nfuncCar :: [Value] -> Value
nfuncCar [Pair x _] = x
nfuncCar _ = err "car" "Must pass pair"

nfuncCdr :: [Value] -> Value
nfuncCdr [Pair _ y] = y
nfuncCdr _ = err "cdr" "Must pass pair"

nfuncEqual :: [Value] -> Value
nfuncEqual [Number x, Number y] = Boolean (x == y)
nfuncEqual _ = err "=" "Must pass two numbers"

nfuncLT :: [Value] -> Value
nfuncLT [Number x, Number y] = Boolean (x < y)
nfuncLT _ = err "<" "Must pass two numbers"

nfuncPlus :: [Value] -> Value
nfuncPlus ns = nfuncPlus' 0 ns where
    nfuncPlus' sofar [] = Number sofar
    nfuncPlus' sofar (Number n:ns) = nfuncPlus' (sofar+n) ns
    nfuncPlus' _ _ = err "+" "All parameters must be numbers"

nfuncMinus :: [Value] -> Value
nfuncMinus [] = err "-" "At least one (number) parameter required"
nfuncMinus [Number n] = Number (-n)
nfuncMinus (Number n:ns) = nfuncMinus' n ns where
    nfuncMinus' sofar [] = Number sofar
    nfuncMinus' sofar (Number n:ns) = nfuncMinus' (sofar-n) ns
    nfuncMinus' _ _ = err "-" "All parameters must be numbers"
nfuncMinus _ = err "-" "All parameters must be numbers"

nfuncIf :: [Value] -> Value
nfuncIf [Boolean True, x, _] = x
nfuncIf [Boolean False, _, y] = y
nfuncIf _ = err "if" "Must pass boolean, two other values"

nfuncCons :: [Value] -> Value
nfuncCons [a, b] = Pair a b
nfuncCons _ = err "cons" "Must pass two parameters"

nfuncList :: [Value] -> Value
nfuncList [] = Null
nfuncList (v:vs) = Pair v (nfuncList vs)

nfuncError :: [Value] -> Value
nfuncError [] = Err ("program-generated\n" ++ "Execution error")
nfuncError [Str msg] = Err ("program-generated\n" ++ msg)
nfuncError _ = err "error" "Must pass no parameters or one string"

nfuncMul :: [Value] -> Value
nfuncMul xs = nfuncMul' 1 xs where
    nfuncMul' num [] = Number num
    nfuncMul' num (Number x:xs) = nfuncMul' (num * x) xs  
    nfuncMul' _ _ = err "*" "All parameters must be numbers"
      
nfuncLTE :: [Value] -> Value
nfuncLTE [Number x, Number y] = Boolean (x<=y)
nfuncLTE _ = err "<=" "Must have two numbers"

nfuncGT :: [Value] -> Value
nfuncGT [Number x, Number y] = Boolean (x>y)
nfuncGT _ = err ">" "Must have two numbers"

nfuncGTE :: [Value] -> Value
nfuncGTE [Number x, Number y] = Boolean (x>=y)
nfuncGTE _ = err ">=" "Must have two numbers"

nfuncQ :: [Value] -> Value
nfuncQ [Number x, Number y] = Number (quot x y)
nfuncQ _ = err "quotient" "Must have two numbers" 

nfuncR :: [Value] -> Value
nfuncR [Number x, Number y] = Number (rem x y)
nfuncR _ = err "remainder" "Must have two numbers" 

nfuncNull :: [Value] -> Value
nfuncNull [Null]    = Boolean True
nfuncNull _ = Boolean False 

nfuncPair :: [Value] -> Value 
nfuncPair [Pair x y]   = Boolean True 
nfuncPair _  = Boolean False 

nfuncNum :: [Value] -> Value
nfuncNum [Number x] = Boolean True
nfuncNum _          = Boolean False 

nfuncStr :: [Value] -> Value 
nfuncStr [Str x]  = Boolean True
nfuncStr _ = Boolean False 

nfuncBool :: [Value] -> Value 
nfuncBool [Boolean x]  = Boolean True
nfuncBool _ = Boolean False 

nfuncStE :: [Value] -> Value
nfuncStE [Str x, Str y]
    | x == y       = Boolean True
    | otherwise    = Boolean False 
nfuncStE _  = err "string=?" "Must have 2 strings"

nfuncSL :: [Value] -> Value
nfuncSL [Str x] = Number (toInteger (length x))
nfuncSL _ = err "string-length" "Must be given a string" 

nfuncAnd :: [Value] -> Value 
nfuncAnd xs = nfuncAnd' True xs where
    nfuncAnd' True [] = Boolean True  
    nfuncAnd' False [] = Boolean False 
    nfuncAnd' False (Boolean x:xs) = Boolean False 
    nfuncAnd' True (Boolean x:xs) = nfuncAnd' (x==True) xs
    nfuncAnd' _ _ = err "and" "Must have list of Booleans"

nfuncOr :: [Value] -> Value 
nfuncOr xs = nfuncOr' False xs where
    nfuncOr' True [] = Boolean True
    nfuncOr' False [] = Boolean False 
    nfuncOr' True (Boolean x:xs) = Boolean True 
    nfuncOr' False (Boolean x:xs) = nfuncOr' (x==True) xs
    nfuncOr' _ _ = err "or" "Must have list of Booleans"

nfuncNot :: [Value] -> Value
nfuncNot [Boolean x] = Boolean (not x)
nfuncNot _ = err "not" "Must be given boolean" 

nfuncMap :: [Value] -> Value 
nfuncMap [Func f, Null] = Null 
nfuncMap [Func f, Pair x xs] = Pair v (nfuncMap [Func f, xs]) where
    (d,v) = f [] [x]
nfuncMap _ = err "map" "Must be given a function and a pair"

nfuncFilter :: [Value] -> Value 
nfuncFilter [Func b, Null] = Null 
nfuncFilter [Func b, Pair z zs] 
    | (nfuncFilter' [Func b, z]) == True       = Pair z (nfuncFilter [Func b, zs])
    | otherwise       = (nfuncFilter [Func b, zs])
nfuncFilter _ = err "filter" "Must be given boolean function, and a pair"

nfuncFilter' [Func b, z] = (v==True) where
    (d,Boolean v) = b [] [z] 

nfuncApply :: [Value] -> Value 
nfuncApply [Func f, Pair x xs] = v where
    (d,v) = f [] (valToList (Pair x xs))
nfuncApply _ = err "apply" "Must be given function and a pair"

nfuncToString :: [Value] -> Value 
nfuncToString [x] = Str (showVal x) 
nfuncToString _ = err "to-string" "Must be given [Value]"

nfuncTake :: [Value] -> Value
nfuncTake [Number n, Pair z zs]
    | n <= 0    = Null
    | otherwise  = Pair z (nfuncTake [Number (n-1), zs])
nfuncTake _ = err "take" "Must be given a number and a pair"

nfuncStCon :: [Value] -> Value
nfuncStCon [Str a, Str b] = Str (a++b)
nfuncStCon [Null, Str b] = Str b
nfuncStCon [Str a, Null] = Str a
nfuncStCon [Null, Null] = err "string-concat" "takes 2 strings"
nfuncStCon _ = err "string-concat" "takes 2 strings"

nfuncStrSub :: [Value] -> Value
nfuncStrSub [Str a, Number pos, Number len] =
    Str (take (fromIntegral len) (drop (fromIntegral pos) a))
nfuncStrSub _ = err "string-sub" "Must have string,number,number"

-- predefs
-- Holds name-value pairs for pre-defined symbols: symbol name, value.
predefs :: DefList
predefs = [
    ("define", Func sfuncDefine),
    ("eval", Func sfuncEval),
    ("quote", Func sfuncQuote),
    ("lambda", Func sfuncLambda),
    ("car", normFunc nfuncCar),
    ("cdr", normFunc nfuncCdr),
    ("=", normFunc nfuncEqual),
    ("<", normFunc nfuncLT),
    ("+", normFunc nfuncPlus),
    ("-", normFunc nfuncMinus),
    ("if", normFunc nfuncIf),
    ("cons", normFunc nfuncCons),
    ("list", normFunc nfuncList),
    ("error", normFunc nfuncError),
    ("*", normFunc nfuncMul),
    ("<=", normFunc nfuncLTE),
    (">", normFunc nfuncGT),
    (">=", normFunc nfuncGTE),
    ("quotient", normFunc nfuncQ),
    ("remainder", normFunc nfuncR),
    ("null?", normFunc nfuncNull),
    ("pair?", normFunc nfuncPair),
    ("number?", normFunc nfuncNum),
    ("string?", normFunc nfuncStr),
    ("boolean?", normFunc nfuncBool), 
    ("string=?", normFunc nfuncStE), 
    ("string-length", normFunc nfuncSL), 
    ("and", normFunc nfuncAnd), 
    ("or", normFunc nfuncOr), 
    ("not", normFunc nfuncNot),
    ("to-string", normFunc nfuncToString),
    ("map", normFunc nfuncMap),
    ("take", normFunc nfuncTake), 
    ("filter", normFunc nfuncFilter), 
    ("apply", normFunc nfuncApply), 
    ("string-concat", normFunc nfuncStCon),
    ("string-sub", normFunc nfuncStrSub)]

