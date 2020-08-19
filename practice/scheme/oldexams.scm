;; Exercises given for CS 403 in past semesters
;; http://cs403.cs.ua.edu

;; Solved by Cody Rivera, Fall 2020

;; Spring 2015

;; #2. logical functions
(define (not x) (if x #f #t))
(define (strict-and x y) (if x y #f))
(define (strict-or x y) (not (strict-and (not x) (not y))))
(define (strict-nand x y) (not (strict-and x y)))
(define (strict-nor x y) (not (strict-or x y)))

;; #3. binary to decimal
(define (binary_to_decimal m) ; Input must be correct
  (if (zero? m)
      0
      (+ (remainder m 10) (* 2 (binary_to_decimal (quotient m 10))))))

;; #4. alist find
(define (alist-find k l)
  (if (null? l)
      #f
      (if (equal? k (caar l))
          (cdar l)
          (alist-find k (cdr l)))))

;; #5. diagonal matrix
(define (diagonal m)
  (if (null? m)
      '()
      (cons (caar m) (diagonal (map cdr (cdr m))))))

;; #8. inner product -- recursive
(define (inner_product X Y f g)
  (if (or (null? (cdr X)) (null? (cdr Y)))
      (g (car X) (car Y))
      (f (g (car X) (car Y))
         (inner_product (cdr X) (cdr Y) f g))))

;; #9. inner product -- list functions
(define (inner_product-list X Y f g)
  (fold-left f
             (g (car X) (car Y))
             (map (lambda (a b) (g a b))
                  (zip (cdr X) (cdr Y)))))

;; #10. foldr map
(define (foldr-map f l)
  (fold-right (lambda (a b) (cons (f a) b)) '() l)))

;; #11. outer product -- recursive
(define (outer_product X Y f)
  (define (help e Y f)
    (if (null? Y)
        '()
        (cons (f e (car Y)) (help e (cdr Y) f))))
  (if (null? X)
      '()
      (cons (help (car X) Y f) (outer_product (cdr X) Y f))))

;; #12. outer product -- list functions
(define (outer_product-list X Y f)
  (map (lambda (a) (map (lambda (b) (f a b)) Y)) X))

   

