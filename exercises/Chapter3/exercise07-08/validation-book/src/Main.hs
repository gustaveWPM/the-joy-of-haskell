-- Exercise (7, 8) — (Minimum length, Combine them all)

module Main (main) where
import Data.Char

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True -> Just xs

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Nothing
    False -> Just password

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Just (x:xs)

validatePassword :: String -> Maybe String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  password <- getLine
  print (validatePassword password)
