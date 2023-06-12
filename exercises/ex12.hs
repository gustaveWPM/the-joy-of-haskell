-- Exercise 12 — The bind function for "StringOrValue"

data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str xs) fun = Str xs
bindStringOrValue (Val x) fun = fun x
