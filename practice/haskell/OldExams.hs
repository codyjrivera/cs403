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
