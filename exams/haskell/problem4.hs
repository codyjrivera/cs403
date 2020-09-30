-- You may modify the given parameters of a function to use pattern matching.

binaryStrings 0 = [""]
binaryStrings n = map ('0' :) bs ++ map ('1' :) bs
  where
    bs = binaryStrings (n - 1)
