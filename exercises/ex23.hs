import Data.List
import Data.Char

-- Exercise 23 — Applicative I/O

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    True -> Nothing
    False ->
      case (all isAlpha word) of
        False -> Nothing
        True -> Just word

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
  case (isWord word1) of
    Nothing -> "The first word is invalid."
    Just word1 ->
      case (isWord word2) of
        Nothing -> "The second word is invalid."
        Just word2 ->
          case (isAnagram word1 word2) of
            False -> "These words are not anagrams."
            True -> "These words are anagrams."

promptWord1 :: IO String
promptWord1 =
  do
    putStr "Please, enter a word.\n> "
    getLine

promptWord2 :: IO String
promptWord2 =
  do
    putStr "Please, enter a second word.\n> "
    getLine

main :: IO ()
main = do
  result <- checkAnagram <$> promptWord1 <*> promptWord2
  print result
