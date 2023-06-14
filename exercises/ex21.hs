import Data.Char

-- Exercise 21 — Fitting in

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

data User = User Username Password
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

checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
  case (length username > 15) of
    True -> Left (Error "Username cannot be longer than 15 characters.")
    False -> Right (Username username)

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
  do
    username2 <- cleanWhitespace username
    username3 <- requireAlphaNum username2
    checkUsernameLength username3

makeUserTmpPassword :: Username -> Either Error User
makeUserTmpPassword username =
  User <$> validateUsername username
       <*> Right (Password "temporaryPassword")

makeUser :: Username -> Password -> Either Error User
makeUser username password =
  User <$> validateUsername username
       <*> validatePassword password

main :: IO ()
main = do
  putStrLn "Please, enter a username:"
  username <- Username <$> getLine
  putStrLn "Please, enter a password:"
  password <- Password <$> getLine 
  print (makeUser username password)
