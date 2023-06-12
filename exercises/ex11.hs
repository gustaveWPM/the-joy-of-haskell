-- Exercise 11 — The bind function for Maybe

bindMaybe :: Maybe a -> (a ->  Maybe b) -> Maybe b
bindMaybe Nothing fun = Nothing
bindMaybe (Just m) fun = fun m
