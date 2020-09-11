
;; Cody Rivera -- CS 403 Exam 1

;; Addition and subtraction
(define (e1-help x)
  (cond ((null? (cdr x)) (car x)) ; last elt is integer
        (else
         (let ((op (cadr x)))
           (if (eqv? op '+)
               (+ (e1-help (cddr x)) (car x))
               (- (e1-help (cddr x)) (car x)))))))

(define (e1 x)
  (e1-help (reverse x)))

;; Multiplication
(define (e2-help x macc)
  (cond ((null? (cdr x)) (list (* macc (car x))))
        (else
         (let ((op (cadr x)))
           (if (eqv? op '*)
               (e2-help (cddr x) (* (car x) macc))
               (cons (* macc (car x))
                     (cons op
                           (e2-help (cddr x) 1))))))))

(define (e2 x)
  (e2-help x 1))

;; Term evaluation
(define (e3 x)
  (e1 (e2 (map (lambda (t)
                 (if (pair? t)
                     (e3 t)
                     t))
               x))))
