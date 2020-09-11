
;; Cody Rivera -- CS 403 Exam 1

(define (type x)
  (cond ((number? x) 'num)
        ((symbol? x) 'sym)
        ((boolean? x) 'bool)
        ((pair? x)
         (cons (type (car x))
               (type (cdr x))))
        (else x))) ; id required for '()

