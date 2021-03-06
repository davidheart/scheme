module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#)(."
----------------------------------------------
-- return of readExpr has a type of LispVal --
-- thus, we have to put this line           --
----------------------------------------------
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
         Left err -> String $ "No match:" ++ show err
         Right val -> val

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

--------------------------------------------------------------------------------
spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many(noneOf "\"")
                 char '"'
                 return $ String x

-- <|> : an operator in the Parse library
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many(letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                             "#t" -> Bool True
                             "#f" -> Bool False
                             otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
            head <- endBy parseExpr spaces
            tail <- char '.' >> spaces >> parseExpr
            return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

-- Exercise : 2.1 rewrite 'parseNumber' with do-notation
-- parseNumber :: Parser LispVal
-- parseNumber = do x <- many1 digit
--                  (return . Number . read) x

-- parseNumber = many1 digit >>= \x -> (return . Number . read) x
-- or
-- parseNumber = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do char ')'
                 x <- (try parseList) <|> parseDottedList
                 char ')'
                 return x

------------------------------------------------------------------------
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents ) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

-- function unwordsList to convert the contained list into a string:--
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
         ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

--------------------------------------------------------------------------------
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val


-- shoot

ddd 















-- some changes are made
