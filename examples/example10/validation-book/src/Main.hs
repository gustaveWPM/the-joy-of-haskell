{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Data.Coerce
import Data.Validation
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- Example 10 — Coercible

newtype Error = Error (NonEmpty String)
  deriving (Semigroup, Show)

newtype Password = Password String
  deriving Show

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show

type Rule a = (a -> Validation Error a)

errorMessage :: String -> Error
errorMessage msg = Error (msg :| [])

display :: Username -> Password -> IO ()
display username password =
  case makeUser username password of
    Failure err -> putStrLn (unlines (NE.toList (coerce err)))
    Success (User username' _) -> putStrLn("Welcome, " ++ coerce username')

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
  case validatePassword password of
    Failure err -> Failure (errorMessage "Invalid password:" <> err)
    Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case validateUsername username of
    Failure err -> Failure (errorMessage "Invalid username:" <> err)
    Success username2 -> Success username2

requireAlphaNum :: Rule String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (errorMessage "Cannot contain whitespaces or special characters.")
    True -> Success xs

cleanWhitespace :: Rule String
cleanWhitespace "" = Failure (errorMessage "Cannot be empty.")
cleanWhitespace (x:xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Success (x:xs)

checkUsernameLength :: Rule Username
checkUsernameLength username =
  case (length (coerce username :: String) > 15) of
    True -> Failure (errorMessage "Username cannot be longer than 15 characters.")
    False -> Success (coerce username)

checkPasswordLength :: Rule Password
checkPasswordLength password =
  case (length (coerce password :: String) < 10 || length (coerce password :: String) > 20) of
    True -> Failure (errorMessage "Your password cannot be longer than 20 characters, and shorter than 10 characters.")
    False -> Success password

validatePassword :: Rule Password
validatePassword (Password password) =
  case (coerce cleanWhitespace :: Rule Password) (Password password) of
    Failure err -> Failure err
    Success password2 -> (coerce requireAlphaNum :: Rule Password) password2 *>
                         checkPasswordLength password2

validateUsername :: Rule Username
validateUsername (Username username) =
  case (coerce cleanWhitespace :: Rule Username) (Username username) of
    Failure err -> Failure err
    Success username2 -> (coerce requireAlphaNum :: Rule Username) username2 *>
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
