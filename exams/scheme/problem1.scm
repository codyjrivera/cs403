
;; Cody Rivera -- CS 403 Exam 1

(define (fetchall x L)
  (map cdr (filter (lambda (n) (equal? x (car n))) L)))
