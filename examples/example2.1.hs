-- Example 2.1 — The anagram checker

import Data.List

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)