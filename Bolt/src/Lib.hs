module Lib where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative 
import Control.Monad
import qualified Data.List as L

-- Writing the definitions of each data type in the JSON specification
-- document(ECMA-404) in terms of the Bolt values.


data BValue = BObject [(String, BValue)]| -- can be identified with {}
              BArray [BValue]| -- can be identified with []
              BNumber Double| 
              BString String|
              BBool Bool|
              BNull deriving (Show)

-- BValue Parser Combinators

-- Parser to remove white spaces
ws :: Parser String
ws = many (oneOf " \t\n\r")

jsonBool :: Parser BValue
jsonBool = BBool <$> (ws *> (((string "true") *> (pure True)) <|> ((string "false") *> (pure False))) <* ws)

jsonNumber :: Parser BValue
jsonNumber = BNumber . (\n -> read n :: Double) <$> (ws *> (many1 digit) <* ws)

jsonNull :: Parser BValue
jsonNull = (ws *> string "null" <* ws) *> pure BNull 

stringLiteral :: Parser String
stringLiteral = ws *> (char '"' *> (many (noneOf ['"'])) <* char '"') <* ws

jsonStringLiteral :: Parser BValue
jsonStringLiteral = BString <$> stringLiteral

jsonArray :: Parser BValue
jsonArray = BArray <$> (ws *> ((char '[') *> (ws *> (jsonValue `sepBy` (char ',')) <* ws) <* (char ']')) <* ws)

jsonObject :: Parser BValue
jsonObject = BObject <$> (ws *> (char '{' *> (ws *> (objectEntry `sepBy` (char ',')) <* ws) <* char '}') <* ws)

objectEntry :: Parser (String, BValue)
objectEntry = do
  key <- stringLiteral
  char ':'
  value <- jsonValue
  return (key, value)

-- Final json parser

jsonValue :: Parser BValue
jsonValue = try jsonBool <|> try jsonNull <|> try jsonNumber <|> try jsonStringLiteral <|> try jsonArray <|> jsonObject

-- fromJson to BValue data type

fromJSON :: String -> BValue
fromJSON s = case (parse jsonValue "" $ s) of
  Right val -> val
  Left err -> error . show $ err 

-- toJSON from BValue data type

toJSON :: BValue -> String
toJSON bValue = case bValue of
  BNull -> "null"
  (BBool True) -> "true"
  (BBool False) -> "false"
  (BNumber n) -> show n
  (BString s) -> "\"" ++ s ++ "\""
  (BArray xs) -> "[" ++ (L.intercalate "," . map toJSON $ xs) ++ "]"
  (BObject ys) -> "{" ++ L.intercalate "," (map (\(k,v) -> ("\"" ++ k ++ "\"") ++ ":" ++ (toJSON v)) ys) ++ "}"

