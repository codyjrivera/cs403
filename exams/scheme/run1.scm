(load "problem1.scm")
(write-line (fetchall 'b '((a . 1) (b . 2) (c . 3) (b . 4) (a . 5) (c . 6) (b . 7) (b . 8) (d . 9))))
(write-line (fetchall 'e '((a . 1) (b . 2) (c . 3) (b . 4) (a . 5) (c . 6) (b . 7) (b . 8) (d . 9))))
(write-line (fetchall 'd '((a . 1) (b . 2) (c . 3) (b . 4) (a . 5) (c . 6) (b . 7) (b . 8) (d . 9))))
