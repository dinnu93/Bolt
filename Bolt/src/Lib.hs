module Lib where

-- Writing the definitions of each data type in the JSON specification
-- document(ECMA-404) in terms of the Bolt values.


data BValue = BObject [(String, BValue)]| -- can be identified with {}
              BArray [BValue]| -- can be identified with []
              BNumber Integer| 
              BString String|
              BBool Bool|
              BNull deriving (Show)

-- Encoding a JSON string into the BValue algebraic data type.

type JSON = String

-- encodeJSON :: JSON -> BValue
-- encodeJSON "true" = BBool True
-- encodeJSON "false" = BBool False
-- encodeJSON "null" = BNull
-- encodeJSON s
--   | isJSONString s = parseJSONString s
--   | isJSONArray s = parseJSONArray s
--   | isJSONObject s = parseJSONObject s
--   | otherwise = "Not a valid JSON String!"
  

-- Predicates for JSON data types

-- Checks if the given string is encapsulated in a symbol like [],{} or "" etc.
encapsulatedIn :: Char -> Char -> String -> Bool 
encapsulatedIn c1 c2 s
  | (length s > 1) && (head s) == c1 && (last s) == c2 = True
  | otherwise = False 
    
    
isJSONString :: JSON -> Bool
isJSONString = encapsulatedIn '"' '"'

isJSONArray :: JSON -> Bool
isJSONArray = encapsulatedIn '[' ']'

isJSONObject :: JSON -> Bool
isJSONObject = encapsulatedIn '{' '}'

-- parsing out JSON data types to Bolt's data types

parseJSONString :: JSON -> BValue
parseJSONString s = BString (drop 1 . take (l-1) $ s) 
  where l = length s
     
  
