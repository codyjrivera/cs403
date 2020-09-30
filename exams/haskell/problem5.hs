
-- You may modify the given parameters of a function to use pattern matching.
-- Do not modify the data definition.

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Read, Show)

t1 = Node 'E' (Node 'B' Nil (Node 'C' Nil Nil)) (Node 'J' (Node 'G' Nil (Node 'H' Nil Nil)) Nil)

t2 = Node 'E' (Node 'B' Nil (Node 'C' Nil Nil)) (Node 'H' (Node 'G' Nil Nil) (Node 'J' Nil Nil))

t3 = Node 'E' (Node 'B' (Node 'A' Nil Nil) (Node 'C' Nil Nil)) (Node 'H' (Node 'G' Nil Nil) (Node 'J' Nil Nil))

height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

unbalanced Nil = 0
unbalanced (Node _ l r) = diff + unbalanced l + unbalanced r
    where
        diff = abs (height r - height l)

