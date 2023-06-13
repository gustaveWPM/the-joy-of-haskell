import Data.Char

-- Example 6.2 — Declaring new types

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

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Left (Error "Your password cannot be longer than 20 characters, and shorter than 10 characters.")
    False -> Right (Password password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
  case (length username > 15) of
    True -> Left (Error "Username cannot be longer than 15 characters.")
    False -> Right (Username username)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  password <- Password <$> getLine
  print (validatePassword password)
