-- Exercise 10 — Doing I/O

reverseLine :: IO ()
-- reverseLine = getline >>= (print . reverse)

reverseLine =
  do
    putStr "Please enter a line\n> "
    line <- getLine
    print (reverse line)
