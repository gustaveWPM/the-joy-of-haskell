-- Exercise 2 — (Bool, Bool)
validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  case (null username, null password) of
    (False, False) -> "Okay"
    (True, False) -> "Empty username"
    (False, True) -> "Empty password"
    (True, True) -> "Empty password and username"
