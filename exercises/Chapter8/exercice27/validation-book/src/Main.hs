{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Char
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Exercice 27 — Strings of text

newtype Username = Username T.Text
  deriving Show

newtype Password = Password T.Text
  deriving Show

newtype Error = Error [T.Text]
  deriving (Semigroup, Show)

data User = User Username Password
  deriving Show

requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum xs =
  case (T.all isAlphaNum xs) of
    False -> Failure (Error ["Cannot contain whitespaces or special characters."])
    True -> Success xs

cleanWhitespace :: T.Text -> Validation Error T.Text
cleanWhitespace input =
  if T.null (T.strip input)
    then Failure (Error ["Cannot be empty."])
    else Success (T.strip input)

checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength username =
  case (T.length username > 15) of
    True -> Failure (Error ["Username cannot be longer than 15 characters."])
    False -> Success (Username username)

checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength password =
  case (T.length password < 10 || T.length password > 20) of
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
  username <- Username <$> T.getLine
  putStrLn "Please, enter a password:"
  password <- Password <$> T.getLine 
  print (makeUser username password)
