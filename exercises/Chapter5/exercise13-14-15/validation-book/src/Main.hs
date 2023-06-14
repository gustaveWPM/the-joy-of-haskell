-- Exercise (13, 14, 15) — (Converting to Either, REPL play, Testing)

module Main (main) where
import Data.Char
import Data.List

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
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines ["Test " ++ show n, " Expected: " ++ show expected, " But got:  " ++ show actual])

printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "") (Left "Your password cannot be longer than 20 characters, and shorter than 10 characters.")
    eq 2 (checkPasswordLength "julielovesbooks")
         (Right "julielovesbooks")
    eq 3 (checkPasswordLength "abcdefghijklmnopqrstuvwxyz")
         (Left "Your password cannot be longer than 20 characters, and shorter than 10 characters.")
    eq 4 (requireAlphaNum "") (Right "")
    eq 5 (requireAlphaNum "julielovesbooks") (Right "julielovesbooks")
    eq 6 (requireAlphaNum "julie loves boks") (Left "Your password cannot contain whitespaces or special characters.")
    eq 7 (cleanWhitespace "     ") (Left "Your passord cannot be empty.")
    eq 8 (cleanWhitespace "     julielovesbooks") (Right "julielovesbooks")

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  password <- getLine
  print (validatePassword password)
