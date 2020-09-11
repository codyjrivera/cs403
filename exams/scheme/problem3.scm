
;; Cody Rivera -- CS 403 Exam 1

(define (cr-help ops l)
  (if (null? ops)
      l
      (if (eqv? (car ops) 'a)
          (cr-help (cdr ops) (car l))
          (cr-help (cdr ops) (cdr l)))))

(define (cr x y)
  (cr-help (reverse x) y))

