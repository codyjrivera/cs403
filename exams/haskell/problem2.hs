-- You may modify the given parameters of a function to use pattern matching.

encode xs ys zs = encode' cb zs
  where
    cb = zip xs ys
    encode' _ [] = []
    encode' cb (z : zs) = case lookup z cb of
      Just cw -> cw : encode' cb zs
