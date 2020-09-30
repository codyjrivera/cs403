-- You may modify the given parameters of a function to use pattern matching.
-- Do not modify the data definition.

roundAwayFromZero x
  | x > 0 = ceiling x
  | x < 0 = floor x
  | otherwise = 0

data Number = I Integer | D Double deriving (Eq, Ord, Read, Show)

plus (I x) (I y) = (I (x + y))
plus (D x) (I y)
  | x == fromIntegral t = (I (t + y))
  | otherwise = (D (x + fromIntegral y))
  where
    t = roundAwayFromZero x
plus (I x) (D y)
  | y == fromIntegral t = (I (x + t))
  | otherwise = (D (fromIntegral x + y))
  where
    t = roundAwayFromZero y
plus (D x) (D y) = (D (x + y))
