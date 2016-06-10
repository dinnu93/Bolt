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

jsonBool :: Parser BValue
jsonBool = BBool <$> (((string "true") *> (pure True)) <|> ((string "false") *> (pure False)))

jsonNumber :: Parser BValue
jsonNumber = BNumber . (\n -> read n :: Double) <$> (many1 digit)

jsonNull :: Parser BValue
jsonNull = string "null" *> pure BNull 

stringLiteral :: Parser String
stringLiteral = char '"' *> (many (noneOf ['"'])) <* char '"'

jsonStringLiteral :: Parser BValue
jsonStringLiteral = BString <$> stringLiteral

jsonArray :: Parser BValue
jsonArray = BArray <$> ((char '[') *> (jsonValue `sepBy` (char ',')) <* (char ']'))

jsonObject :: Parser BValue
jsonObject = BObject <$> (char '{' *> objectEntry `sepBy` (char ',') <* char '}')

objectEntry :: Parser (String, BValue)
objectEntry = do
  key <- stringLiteral
  char ':'
  value <- jsonValue
  return (key, value)

-- Remove Escapes " \t\n\r"

removeEscapes :: String -> String
removeEscapes s = map snd . filter leaveStr $ iList
  where l = length s
        iList = zip [0..(l-1)] s
        strIndex = giveRange . map fst . filter (\(i,e) -> e == '"') $ iList
        leaveStr (i,e)
          | elem i strIndex = True
          | elem e " \t\n\r" = False
          | otherwise = True
        
giveRange :: [Int] -> [Int]
giveRange [] = []
giveRange (x:y:xs) = [x..y] ++ giveRange xs

-- Final json parser

jsonValue :: Parser BValue
jsonValue = jsonBool <|> jsonNull <|> jsonNumber <|> jsonStringLiteral <|> jsonArray <|> jsonObject

-- fromJson to BValue data type

fromJSON :: String -> BValue
fromJSON s = case (parse (jsonValue <* eof) "" $ removeEscapes s) of
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

