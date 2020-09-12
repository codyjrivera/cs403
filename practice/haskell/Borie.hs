-- |
-- Exercises directly suggested by Richard Borie
-- http://cs403.cs.ua.edu/fall2020/exercises.htm
--
-- Solved by Cody Rivera, Fall 2020
module Borie where

import Prelude

-- | #1. Exponentiation
power :: (Num m, Integral n) => m -> n -> m
power _ 0 = 1
power m n
  | odd n = m * power m (n - 1)
  | otherwise = s * s
  where
    s = power m (n `div` 2)

-- | #2. Logarithm
blog :: (Ord m, Num m, Integral n) => m -> m -> n
blog m q = logHelp m q 0
  where
    logHelp m q n
      | m `power` n >= q = n
      | otherwise = logHelp m q (n + 1)

-- | #3. Combinations
comb :: (Integral n) => n -> n -> n
comb 0 _ = 1
comb _ 0 = 1
comb n k
  | n == k = 1
  | otherwise = comb (n - 1) k + comb (n - 1) (k - 1)

-- | Ordered list insert
insertList :: (Ord n) => n -> [n] -> [n]
insertList n [] = [n]
insertList n (x : xs)
  | n <= x = n : (x : xs)
  | otherwise = x : (insertList n xs)

-- | #4. Insertion sort
insertionSort :: (Ord n) => [n] -> [n]
insertionSort [] = []
insertionSort (x : xs) = insertList x (insertionSort xs)

-- | #5. Selection sort
selectionSort :: (Ord n) => [n] -> [n]
selectionSort [] = []
selectionSort xs = e : (selectionSort $ filter (/= e) xs)
  where
    e = minimum xs

-- | Merge
merge :: (Ord n) => [n] -> [n] -> [n]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : (merge xs (y : ys))
  | otherwise = y : (merge (x : xs) ys)

-- | #6. Merge sort
mergeSort :: (Ord n) => [n] -> [n]
mergeSort [] = []
mergeSort [x] = [x] -- Edge case beware
mergeSort xs = merge (mergeSort ls) (mergeSort rs)
  where
    (ls, rs) = splitAt (l `div` 2) xs
    l = length xs

-- | Quick sort pivot - deterministic
quickSortPivot :: [n] -> (n, [n])
quickSortPivot (x : xs) = (x, xs)

-- | #7. Quick sort
quickSort :: (Ord n) => [n] -> [n]
quickSort [] = []
quickSort xs = quickSort ls ++ [p] ++ quickSort rs
  where
    (p, xr) = quickSortPivot xs
    ls = [l | l <- xr, l <= p]
    rs = [r | r <- xr, r > p]

-- | BST Definition
data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show)

-- | #8. BST membership
member :: (Ord a) => a -> Tree a -> Bool
member _ Nil = False
member a (Node k l r)
  | a == k = True
  | a < k = member a l
  | otherwise = member a r

-- | #9. BST insert
insert :: (Ord a) => a -> Tree a -> Tree a
insert a Nil = Node a Nil Nil
insert a (Node k l r)
  | a == k = Node k l r
  | a < k = Node k (insert a l) r
  | otherwise = Node k l (insert a r)

-- | BST max
bstMax :: (Ord a) => Tree a -> a
bstMax (Node a _ Nil) = a
bstMax (Node a _ r) = bstMax r

-- | #10. BST delete
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Nil = Nil
delete a (Node k l r)
  | a == k = help l r
  | a < k = Node k (delete a l) r
  | otherwise = Node k l (delete a r)
  where
    help l r = case (l, r) of
      (Nil, Nil) -> Nil
      (l, Nil) -> l
      (Nil, r) -> r
      (l, r) -> Node m (delete m l) r
      where
        m = bstMax l

-- | apply each
applyEach :: [(a -> b)] -> [a] -> [b]
applyEach [] _ = []
applyEach (f : fs) (x : xs) = f x : applyEach fs xs
