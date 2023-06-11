-- Example 2.2 — The word validator

import Data.Char
import Data.List

isWord :: String -> Maybe String
case (null word) of
  True -> Nothing
  False ->
    case (all isAlpha word) of
      False -> Nothing
      True -> Just word
