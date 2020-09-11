(load "problem2.scm")
(write-line (type '(2 a #t (3 b (4 . #f) c) (5 d . 6))))
(write-line (type '()))
(write-line (type (cons 42 '())))

