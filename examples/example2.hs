import Data.List
import Data.Char

-- Example 2.1 — The anagram checker

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

-- Example 2.2 — The word validator

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    True -> Nothing
    False ->
      case (all isAlpha word) of
        False -> Nothing
        True -> Just word

-- Example 2.3 — Validate first, then compare

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

-- Example 2.4 — Interactive program

main :: IO ()
main =
  do
    putStr "Please enter a word\n> "
    word1 <- getLine
    putStr "Please enter a second word\n> "
    word2 <- getLine
    print (checkAnagram word1 word2)
