-- You may modify the given parameters of a function to use pattern matching.

subseq' [] _ = []
subseq' ((xi, x) : xs) (i : is)
  | xi == i = x : subseq' xs is
  | otherwise = subseq' xs (i : is)

subseq f xs = subseq' (zip [0 ..] xs) (map f [0 ..])
