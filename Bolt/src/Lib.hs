module Lib where

import Data.String.Utils
import Data.Char
import qualified Data.Map as Map
-- Writing the definitions of each data type in the JSON specification
-- document(ECMA-404) in terms of the Bolt values.


data BValue = BObject (Map.Map String BValue)| -- can be identified with {}
              BArray [BValue]| -- can be identified with []
              BNumber Double| 
              BString String|
              BBool Bool|
              BNull deriving (Show)

-- Encoding a JSON string into the BValue algebraic data type.

encodeJSON :: String -> BValue
encodeJSON s
  | stripped == "true" = BBool True
  | stripped == "false" = BBool False
  | stripped == "null" = BNull
  | isJSONNumber stripped = parseJSONNumber stripped
  | isJSONString stripped = parseJSONString stripped 
  | isJSONArray stripped = parseJSONArray stripped 
  | isJSONObject stripped = parseJSONObject stripped 
  | otherwise = error "Not a valid JSON String!"
  where stripped = strip . stripLines $ s

-- Predicates for JSON data types

-- Checks if the given string is encapsulated in a symbol like [],{} or "" etc.
encapsulatedIn :: Char -> Char -> String -> Bool 
encapsulatedIn c1 c2 s
  | (length s > 1) && (head s) == c1 && (last s) == c2 = True
  | otherwise = False 

stripLines :: String -> String
stripLines s = join " " . lines $ s

strippedString :: String -> String
strippedString s = tail . init $ stripStr
  where stripStr = strip s
        l = length stripStr
    
isJSONString :: String -> Bool
isJSONString = encapsulatedIn '"' '"' . strip 

isJSONNumber :: String -> Bool
isJSONNumber = and . map isNumber . strip   

isJSONArray :: String -> Bool
isJSONArray = encapsulatedIn '[' ']' . strip 

isJSONObject :: String -> Bool
isJSONObject = encapsulatedIn '{' '}' . strip 

-- parsing out JSON data types to Bolt's data types

parseJSONString :: String -> BValue
parseJSONString s = BString $ strippedString s
  
parseJSONNumber :: String -> BValue
parseJSONNumber s = BNumber (read $ strip s :: Double) 

parseJSONArray :: String -> BValue
parseJSONArray s = BArray (map encodeJSON . split "," . strippedString . stripLines $ s)

parseJSONPair :: String -> (String, BValue)
parseJSONPair s = (strippedString (key ++ "\""), encodeJSON value)
  where [key,value] = map strip . split "\":" . strip $ s

parseJSONObject :: String -> BValue
parseJSONObject s
  | l == 0 = BObject (Map.fromList [])
  | otherwise = BObject (Map.fromList . map parseJSONPair $ objList)
  where preObjList = split "\"," . strippedString . stripLines $ s
        l = length preObjList
        objList = map (++"\"") (init preObjList) ++ [last preObjList]

