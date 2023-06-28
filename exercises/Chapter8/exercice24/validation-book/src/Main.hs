-- Exercice 24 — Either is fine

module Main (main) where
import Data.Char

requireAlphaNum :: String -> Either String String
requireAlphaNum password =
  case (all isAlphaNum password) of
    False -> Left "Your password cannot contain whitespaces or special characters."
    True -> Right password

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Left "Your password cannot be longer than 20 characters, and shorter than 10 characters."
    False -> Right password

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Your passord cannot be empty."
cleanWhitespace (x:xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Right (x:xs)

validatePassword :: String -> Either String String
validatePassword password = 
  case cleanWhitespace password of
    Left err -> Left err
    Right password2 -> requireAlphaNum password2 *>
                       checkPasswordLength password2

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  password <- getLine
  print (validatePassword password)
