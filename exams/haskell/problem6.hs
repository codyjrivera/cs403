
-- You may modify the given parameters of a function to use pattern matching.
-- Do not modify the data definition.

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Read, Show)

createTree [] = Nil
createTree [x] = Node x Nil Nil
-- Unbalance prefered on right
createTree [x0, x1] = Node x0 Nil (Node x1 Nil Nil)
createTree xs = Node (xs !! mid) (createTree (take mid xs)) (createTree (drop (mid + 1) xs))
    where
        mid = (length xs - 1) `div` 2

