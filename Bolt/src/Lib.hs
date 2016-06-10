module Lib where

import qualified Data.Map as Map
-- Writing the definitions of each data type in the JSON specification
-- document(ECMA-404) in terms of the Bolt values.


data BValue = BObject (Map.Map String BValue)| -- can be identified with {}
              BArray [BValue]| -- can be identified with []
              BNumber Double| 
              BString String|
              BBool Bool|
              BNull deriving (Show)

