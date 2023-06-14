import Data.Char

-- Exercise 6 — Hacker voice

substituteChar :: Char -> Char
substituteChar 'G' = '6'
substituteChar c = case toLower c of
  'o' -> '0'
  'i' -> '1'
  'e' -> '3'
  'a' -> '4'
  's' -> '5'
  't' -> '7'
  'b' -> '8'
  'g' -> '9'
  _   -> c

translateWord :: String -> String
translateWord word = map substituteChar word

main :: IO ()
main =
  do
    putStr "Please enter a word\n> "
    word <- getLine
    print (translateWord word)
