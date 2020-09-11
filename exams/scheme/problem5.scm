
;; Cody Rivera -- CS 403 Exam 1

(define (vv f g x y)
  (let ((temp (map (lambda (args) (apply g args))
                   (zip x y))))
    (fold-left f (car temp) (cdr temp))))

(define (vm f g x y)
  (let ((vectors (apply zip y)))
    (map (lambda (vector) (vv f g x vector))
         vectors)))

(define (mv f g x y)
  (map (lambda (vector) (vv f g vector y)) x))
       
(define (mm f g x y)
  (map (lambda (vector) (vm f g vector y)) x))


