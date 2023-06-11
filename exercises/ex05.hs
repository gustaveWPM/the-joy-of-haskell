import Data.List
import Data.Char
import Data.List

-- Exercise 5 — Palindromes

isPalindrome :: String -> Bool
isPalindrome word = (reverse word) == word

isWord :: String -> Maybe String
isWord word =
  case (filter isAlpha word) of
    [] -> Nothing
    word -> Just (map toLower word)

checkPalindrome :: String -> String
checkPalindrome word =
  case (isWord word) of
    Nothing -> "The word is invalid."
    Just word ->
      case (isPalindrome word) of
        False -> "This word is not a palindrome."
        True -> "This word is a palindrome."

main :: IO ()
main =
  do
    putStr "Please enter a word\n> "
    word <- getLine
    print (checkPalindrome word)
