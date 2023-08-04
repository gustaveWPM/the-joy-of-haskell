{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Data.Validation

-- Example 9 — Better error messages

newtype Error = Error [String]
  deriving (Semigroup, Show)

newtype Password = Password String
  deriving Show

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show

errorCoerce :: Error -> [String]
errorCoerce (Error err) = err

display :: Username -> Password -> IO ()
display username password =
  case makeUser username password of
    Failure err -> putStr (unlines (errorCoerce err))
    Success (User (Username username') _) -> putStrLn("Welcome, " ++ username')

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
  case validatePassword password of
    Failure err -> Failure (Error ["Invalid password:"] <> err)
    Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case validateUsername username of
    Failure err -> Failure (Error ["Invalid username:"] <> err)
    Success username2 -> Success username2

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (Error ["Cannot contain whitespaces or special characters."])
    True -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty."])
cleanWhitespace (x:xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Success (x:xs)

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength username =
  case (length username > 15) of
    True -> Failure (Error ["Username cannot be longer than 15 characters."])
    False -> Success (Username username)

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Failure (Error ["Your password cannot be longer than 20 characters, and shorter than 10 characters."])
    False -> Success (Password password)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *>
                         checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case (cleanWhitespace username) of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *>
                         checkUsernameLength username2

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> usernameErrors username
       <*> passwordErrors password

main :: IO ()
main = do
  putStrLn "Please, enter a username:"
  username <- Username <$> getLine
  putStrLn "Please, enter a password:"
  password <- Password <$> getLine 
  display username password
