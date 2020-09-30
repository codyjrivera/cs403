-- |
-- Exams given for CS 403 in past semesters
-- http://cs403.cs.ua.edu
--
-- Solved by Cody Rivera, Fall 2020
module OldExams where

import Prelude

-- Spring 2015

-- | #1. Convert
convert :: ([a], [a], [a]) -> [(a, a, a)]
convert (x : xs, y : ys, z : zs) = (x, y, z) : convert (xs, ys, zs)
convert _ = [] -- Fallthrough case - length of shortest list

-- | #2. Invert
invert :: [(a, a, a)] -> ([a], [a], [a])
invert [] = ([], [], [])
invert ((x, y, z) : ps) = (x : xs, y : ys, z : zs)
  where
    (xs, ys, zs) = invert ps

-- | #3. Infinite list find
find :: (Ord a) => a -> [a] -> Bool
find _ [] = False
find v (x : xs)
  | v == x = True
  | v > x = find v xs
  | otherwise = False

-- | #4. Split range
splitRange :: (Ord a) => a -> a -> [a] -> ([a], [a])
splitRange _ _ [] = ([], [])
splitRange x y (z : zs)
  | x <= z && z <= y = ((z : is), es)
  | otherwise = (is, (z : es))
  where
    (is, es) = splitRange x y zs

-- | #5. Repeated application
loop :: (Integral b) => (a -> a) -> a -> b -> a
loop _ s 0 = s
loop f s n = loop f (f s) (n - 1)

-- * #6. Redevelop arithmetic on positive Integrals

inc :: Num a => a -> a
inc x = x + 1

addHelp :: (Integral a) => a -> a -> a -> a
addHelp cy y s
  | cy < y = addHelp (inc cy) y (inc s)
  | otherwise = s

add :: (Integral a) => a -> a -> a
add x y = addHelp 0 y x

multHelp :: (Integral a) => a -> a -> a -> a -> a
multHelp x cy y s
  | cy < y = multHelp x (inc cy) y (add s x)
  | otherwise = s

mult :: (Integral a) => a -> a -> a
mult x 0 = 0
mult x y = multHelp x 0 y 0

-- Ad nauseum

-- | #7. Extremes
extremes :: [[a]] -> [[a]]
extremes [] = []
extremes ((e : es) : xs) = [e, last es] : extremes xs

-- * #8. Curry and uncurry

-- | Curry
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

-- | Uncurry
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f t = f a b c
  where
    (a, b, c) = t

-- | Expression Type
data Expr
  = BinaryOp (Int -> Int -> Int) Expr Expr
  | UnaryOp (Int -> Int) Expr
  | Value Int

-- | #9. Expression Evaluate
eval :: Expr -> Int
eval (Value v) = v
eval (UnaryOp f x) = f (eval x)
eval (BinaryOp f x y) = f (eval x) (eval y)

-- | #10. Suffixes
suffixes :: [a] -> [[a]]
suffixes xs = reverse (help xs)
  where
    help [] = [[]]
    help (x : xs) = (x : xs) : help xs

-- | #11. Prefixes
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x : xs) = [] : map (x :) (prefixes xs)

-- * #12 omitted

-- Fall 2015

-- | #1. Count
count :: (Eq a, Integral b) => a -> [a] -> b
count _ [] = 0
count v (x : xs)
  | v == x = 1 + count v xs
  | otherwise = count v xs

-- | #2. Partition
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs)
  | p x = ((x : ys), zs)
  | otherwise = (ys, (x : zs))
  where
    (ys, zs) = partition p xs

-- * #3. Strict logical functions

-- | Strict AND
(&&&) :: Bool -> Bool -> Bool
(&&&) False False = False
(&&&) False True = False
(&&&) True False = False
(&&&) True True = True

-- | Strict OR
(|||) :: Bool -> Bool -> Bool
(|||) False False = False
(|||) False True = True
(|||) True False = True
(|||) True True = True

-- * #4. Redefine arithmetic

-- | Natural Numbers
data Natural = Zero | Successor Natural
  deriving (Show, Eq)

-- | Natural addition
add' :: Natural -> Natural -> Natural
add' Zero b = b
add' (Successor a) b = Successor (add' a b)

-- | Natural subtraction
sub' :: Natural -> Natural -> Natural
sub' a Zero = a
sub' Zero _ = Zero
sub' (Successor a) (Successor b) = sub' a b

-- | Natural multiplication
mul' :: Natural -> Natural -> Natural
mul' _ Zero = Zero
mul' a (Successor b) = add' a (mul' a b)

-- | #5. Perfect number detection
isPerfect :: Integral a => a -> Bool
isPerfect n = sum (filter (\x -> (n `mod` x) == 0) [1 .. n -1]) == n

-- | #6. All rotations - O(n^2)
rotations :: [a] -> [[a]]
rotations xs = rotIter xs 0 $ length xs
  where
    rotIter xs n l
      | n >= l = []
      | otherwise = xs : rotIter (tail xs ++ [head xs]) (n + 1) l

-- * #7. Trees

-- | General tree structure
data Tree a = Node a [Tree a]

-- | Preorder
preorder :: Tree a -> [a]
preorder (Node a []) = [a]
preorder (Node a xs) = [a] ++ (foldl (++) [] (map preorder xs))

-- | Postorder
postorder :: Tree a -> [a]
postorder (Node a []) = [a]
postorder (Node a xs) = (foldl (++) [] (map postorder xs)) ++ [a]

t :: Tree Char
t = Node 'M' [Node 'K' [Node 'Q' [], Node 'E' []], Node 'X' [], Node 'H' [Node 'U' []]]

-- | #8. Transpose
transpose :: [[a]] -> [[a]]
transpose [r] = map (: []) r
transpose (r : rs) = map (uncurry (:)) $ zip r (transpose rs)

-- | #9. Intersection infinite lists
intersect :: Ord a => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []
intersect (x : xs) (y : ys)
  | x < y = intersect xs (y : ys)
  | x > y = intersect (x : xs) ys
  | otherwise = x : intersect xs ys

-- | #10. Combination
combine :: (b -> b -> c) -> (a1 -> b) -> (a2 -> b) -> [a1] -> [a2] -> [c]
combine f g h xs ys = map (uncurry f) $ zip (map g xs) (map h ys)

-- Spring 2017

-- | #1. Apply3
apply3 :: (a -> a', b -> b', c -> c') -> (a, b, c) -> (a', b', c')
apply3 (f, g, h) (a, b, c) = (f a, g b, h c)

-- | #2. Series
iseries :: a -> (a -> a) -> [a]
iseries s f = s : iseries (f s) f

series :: a -> (a -> a) -> Int -> [a]
series s f n = take n (iseries s f)

-- | #3. Sqrt
mysqrt :: Integral a => a -> a
mysqrt n = f 0 n
  where
    f u l
      | m * m > n = f l (m - 1)
      | (m + 1) * (m + 1) < n = f (m + 1) u
      | otherwise = m
      where
        m = (l + u) `div` 2

-- | #4. Factorize -- Borie's solution
factorize :: Integral a => a -> [a]
factorize n = factorize' n 2
  where
    factorize' n m =
      if m ^ 2 > n
        then [n]
        else
          if mod n m == 0
            then m : factorize' (div n m) m
            else factorize' n (m + 1)

-- | #5. Power set
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = ps ++ map (x :) ps
  where
    ps = powerset xs

-- * #6. non-empty folding

-- | foldl
foldl' :: (a -> a -> a) -> [a] -> a
foldl' f (x : xs) = foldl'' f xs x
  where
    foldl'' _ [] a = a
    foldl'' f (x : xs) a = foldl'' f xs (f a x)

-- | foldr
foldr' :: (a -> a -> a) -> [a] -> a
foldr' f [x] = x
foldr' f (x : xs) = f x (foldr' f xs)

-- | Tree 2nd def
data Tree' a = Leaf a | Branch [Tree' a]
  deriving (Show)

t1 :: Tree' Integer
t1 = Branch [Leaf 3, Branch [Leaf 4, Branch [Leaf 5, Leaf 6], Branch []], Leaf 7]

t2 :: Tree' Char
t2 = Branch [Leaf 'a', Branch [Leaf 'b', Leaf 'c'], Leaf 'd', Branch [Leaf 'e', Leaf 'f'], Leaf 'g']

-- | #7. mapall
mapall :: (a -> b) -> Tree' a -> Tree' b
mapall f (Leaf l) = Leaf (f l)
mapall f (Branch xs) = Branch (map (mapall f) xs)

-- | #8. level
level :: Integral a => a -> Tree' b -> [b]
level n t = level' 0 t
  where
    level' l (Leaf a)
      | l == n = [a]
      | otherwise = []
    level' l (Branch a)
      | l == n = []
      | otherwise = foldl (++) [] (map (level' (l + 1)) a)

-- | #9. table
table :: Integral a => (a -> a -> b) -> [[b]]
table f = [[f x y | y <- [0..]] | x <- [0..]]

-- | subtable
subtable :: Int -> Int -> [[a]] -> [[a]]
subtable r c t = take r (map (take c) t)

-- | #10. column
column :: Int -> [[a]] -> [a]
column k t = map (!! k) t

-- | diagonal
diagonal t = map (\c -> map (!! c) t) [0..]