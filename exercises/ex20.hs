import Data.Char

-- Exercice 20 — Doing Either

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left (Error "Cannot contain whitespaces or special characters.")
    True -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Cannot be empty.")
cleanWhitespace (x:xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Right (x:xs)

checkLength :: Int -> String -> Either Error String
checkLength int str =
  case (length str > int) of
    True -> Left (Error "Input exceeded maximum allowed length.")
    False -> Right str

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Left (Error "Your password cannot be longer than 20 characters, and shorter than 10 characters.")
    False -> Right (Password password)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  do
    password2 <- cleanWhitespace password
    password3 <- requireAlphaNum password2
    checkPasswordLength password3

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  Username <$>
  (cleanWhitespace username
    >>= requireAlphaNum
    >>= checkLength 15)

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  >> (Password <$> getLine)
  >>= \password -> print (validatePassword password)

