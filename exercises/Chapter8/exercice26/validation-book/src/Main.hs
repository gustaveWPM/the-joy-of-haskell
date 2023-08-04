{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Char
import Data.Validation

-- Exercise 26 — Applicative do

newtype Password = Password String
  deriving Show

newtype Error = Error [String]
  deriving (Semigroup, Show)

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show

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
  do
    username2 <- validateUsername username
    password2 <- validatePassword password
    pure (User username2 password2)

main :: IO ()
main = do
  putStrLn "Please, enter a username:"
  username <- Username <$> getLine
  putStrLn "Please, enter a password:"
  password <- Password <$> getLine 
  print (makeUser username password)
