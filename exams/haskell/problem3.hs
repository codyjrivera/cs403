-- You may modify the given parameters of a function to use pattern matching.

-- Incorrect solution -- this solution assumes f is strictly increasing
{- subseq' [] _ = []
subseq' ((xi, x) : xs) (i : is)
  | xi == i = x : subseq' xs is
  | otherwise = subseq' xs (i : is)

subseq f xs = subseq' (zip [0 ..] xs) (map f [0 ..]) -}

-- I place the instructor's solution here, which works generally (at O(n^2) complexity),
-- see http://cs403.cs.ua.edu/fall2020/exam2key.pdf
subseq f xs = help 0
  where
    help k =
      if (f k < 0) || null (drop (f k) xs)
        then []
        else (xs !! f k) : help (k + 1)