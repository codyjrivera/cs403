-- You may modify the given parameters of a function to use pattern matching.

walk x ps = walk' x ps []
  where
    walk' x ps vs
      | elem x vs = 0
      | otherwise = case lookup x ps of
        Just e -> walk' e ps (x : vs)
        Nothing -> x
