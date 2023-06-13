import Data.Char

-- Exercice 18 — Generalizing with a parameter

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

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  Password <$>
  (cleanWhitespace password
    >>= requireAlphaNum
    >>= checkLength 20)

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  Username <$>
  (cleanWhitespace username
    >>= requireAlphaNum
    >>= checkLength 15)

main :: IO ()
main = do
  putStrLn "Please, enter a password:"
  password <- Password <$> getLine
  print (validatePassword password)
