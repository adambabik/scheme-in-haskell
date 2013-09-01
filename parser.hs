module Main where
import Numeric
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
	args <- getArgs
	putStrLn $ readExpr (args !! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
						 | List [LispVal]
						 | DottedList [LispVal] LispVal
						 | Number Integer
						 | String String
						 | Bool Bool
						 | Character Char
						 | Float Double
						 | Ratio Rational
						 | Complex (Complex Double)
						 deriving Show

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found value" ++ show val

escapedChars :: Parser Char
escapedChars = do
	char '\\'
	x <- oneOf "\\\"nrt"
	return $ case x of
		'\\' -> x
		'"' -> x
		'n' -> '\n'
		'r' -> '\r'
		't' -> '\t'

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many $ escapedChars <|> noneOf "\"\\"
	char '"'
	return $ String x

parseAtom :: Parser LispVal
parseAtom = many1 (letter <|> digit <|> symbol) >>= return . String

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= return . Number . read

parseDigital2 :: Parser LispVal
parseDigital2 = do
	try $ string "#d"
	many1 digit >>= return . Number . read

hex2dig x = fst $ readHex x !! 0

parseHex :: Parser LispVal
parseHex = do
	try $ string "#x"
	many1 hexDigit >>= return . Number . hex2dig

oct2dig x = fst $ readOct x !! 0

parseOct :: Parser LispVal
parseOct = do
	try $ string "#o"
	many1 octDigit >>= return . Number . oct2dig

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseBin :: Parser LispVal
parseBin = do
	try $ string "#b"
	x <- many1 (oneOf "10")
	return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseBool :: Parser LispVal
parseBool = do
	char '#'
	x <- oneOf "tf"
	notFollowedBy anyChar -- if bool it cannot be followed by any char
	return $ case x of
		't' -> Bool True
		'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
	string "#\\"
	value <- (string "newline" <|> string "space")
		<|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
	return $ Character $ case value of
		"space" -> ' '
		"newline" -> '\n'
		otherwise -> value !! 0

parseFloat :: Parser LispVal
parseFloat = do
	x <- many1 digit
	char '.'
	y <- many1 digit
	return $ Float $ fst $ head $ readFloat (x ++ '.' ++ y)

parseRatio :: Parser LispVal
parseRatio = do
	x <- many1 digit
	char '/'
	y <- many1 digit
	return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble Float f = f
toDouble Number n = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
	x <- try $ parseFloat <|> parseNumber
	char '+'
	y <- try $ parseFloat <|> parseNumber
	char 'i'
	return $ Complex (toDouble x :+ toDouble y)

parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> try parseFloat
	<|> try parseRatio
	<|> try parseComplex
	<|> try parseNumber -- using try because all can start with # and it should not be consumed
	<|> try parseBool
	<|> try parseCharacter
