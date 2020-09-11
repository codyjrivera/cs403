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

;; Fall 2015

;; #2. sum of products
(define (sum_of_products l)
  (define (mul-list l)
    (if (null? l)
        1
        (* (car l) (mul-list (cdr l)))))
  (if (null? l)
      0
      (+ (mul-list (car l)) (sum_of_products (cdr l)))))

;; #3. sum of products map, fold
(define (sum_of_products-lf l)
  (fold-left + 0 (map (lambda (l) (fold-left * 1 l)) l)))

;; #4. scan
(define (scan op id L)
  (define (scan-help r i)
    (if (null? i)
        (list r)
        (cons r (scan-help (op r (car i)) (cdr i)))))
  (scan-help id L))

;; #8. map with fold-left
(define (fold-left-map f l)
  (fold-left (lambda (a b) (append a (list (f b)))) '() l))

;; #9. a2p
(define (a2p alist)
  (if (null? alist)
      '()
      (let ((elt (car alist)))
        (cons (car elt)
              (cons (cdr elt) (a2p (cdr alist)))))))

;; #10. p2a
(define (p2a plist) ; plist must have even length
  (if (null? plist)
      '()
      (cons (cons (car plist)
                  (cadr plist))
            (p2a (cddr plist)))))

;; Spring 2017

;; #1. sequence
(define (sequence s f n)
  (define (help c i)
    (if (< i n)
        (cons c (help (f c) (+ i 1)))
        '()))
  (help s 0))

;; #2. twist
(define (twist L)
  (define (help l)
    (if (null? l)
        '()
        (if (list? (car l))
            (cons (twist (car l)) (help (cdr l)))
            (cons (car l) (help (cdr l))))))
  (reverse (help L)))

;; #3. countall
(define (countall l)
  (cond ((null? l) 0)
        ((not (pair? l)) 1)
        (else (fold-left + 0 (map countall l)))))

;; #4. mapall
(define (mapall f l)
  (cond ((null? l) '())
        ((not (pair? l)) (f l))
        (else (map (lambda (l) (mapall f l)) l))))

;; #5. efficient square root
(define (mysqrt n)
  (define (search l u)
    (if (> l u)
        u
        (let ((mid (quotient (+ l u) 2)))
          (cond ((= n (square mid)) mid)
                ((< n (square mid))
                 (search l (- mid 1)))
                (else
                 (search (+ mid 1) u))))))
  (search 0 n))

;; #6. prime
(define (prime? n)
  (define (prime-search n x)
    (cond ((= x 1) #t)
          ((= (remainder n x) 0) #f)
          (else (prime-search n (- x 1)))))
  (and (>= n 2) (prime-search n (mysqrt n))))

;; #7. factorize
(define (factorize-help n a)
  (define (search-prime n x)
    (if (and (prime? x) (= (remainder n x) 0))
        x
        (search-prime n (- x 1))))
  (if (< n 2)
      a
      (let ((p (if (prime? n) n (search-prime n (mysqrt n)))))
        (factorize-help (quotient n p)
                        (cons p a)))))

(define (factorize n)
  (factorize-help n '()))

;; #8. power set
(define (powerset S)
  (cond ((null? S) '(()))
        (else
         (let ((sp (powerset (cdr S))))
           (append (map (lambda (e) (cons (car S) e)) sp)
                   sp)))))

;; #9. quantifiers
(define (forall p . a)
  (if (null? a)
      #t
      (and (p (car a)) (apply forall (cons p (cdr a))))))

(define (exists p . a)
  (if (null? a)
      #f
      (or (p (car a)) (apply exists (cons p (cdr a))))))

;; #10. HOF predicate sets

;; provided
(define (member? x S) (S x))
(define (nullset) (lambda (x) #f))
(define (universalset) (lambda (x) #t))
(define (difference S T) (intersect S (complement T)))

;; solutions
(define (add e S)
  (lambda (x)
    (or (equal? x e)
        (S x))))

(define (remove e S)
  (lambda (x)
    (if (equal? x e)
        #f
        (S x))))

(define (union S T)
  (lambda (x)
    (or (S x) (T x))))

(define (intersect S T)
  (lambda (x)
    (and (S x) (T x))))

(define (complement S)
  (lambda (x)
    (not (S x))))

;; Spring 2018 Bonus Quiz

;; #1. scheme flatten
(define (flatten L)
  (define (help L R)
    (if (null? L) R
        (if (not (pair? L))
            (cons L R)
            (help (car L) (help (cdr L) R))))))

;; Spring 2018

;; #1. sum of fourth powers of odd positive numbers
(define (hypercube n) (* n n n n))

(define (fun L)
  (if (null? L)
      0
      (if (and (odd? (car L)) (> (car L) 0))
          (+ (hypercube (car L)) (fun (cdr L)))
          (fun (cdr L)))))

;; #2. matrix gen
(define (matrix r c x)
  (define (matrix-row l u s)
    (if (< l u)
        (cons (+ s l) (matrix-row (+ l 1)
                                  u
                                  s))
        '()))
  (define (iter-rows l u s)
    (if (< l u)
        (cons (matrix-row 0 c s)
              (iter-rows (+ l 1)
                         u
                         (+ s c)))
        '()))
  (iter-rows 0 r x))

;; #3. transpose matrix
(define (transpose M)
  (if (null? (car M))
      '()
      (cons (map car M) (transpose (map cdr M)))))

;; #5. object w/mutable state
(define (counter)
  (let ((val 0))
    (lambda (m)
      (cond ((eq? m 'increment) (set! val (+ val 1)) val)
            ((eq? m 'decrement) (set! val (- val 1)) val)
            (else 'error)))))

;; #6. evaluator
(define (lookup i L)
  (if (null? L)
      (error "Not Found")
      (if (eqv? i (caar L))
          (cdar L)
          (lookup i (cdr L)))))

(define (evaluate e L)
  (cond ((number? e) e)
        ((symbol? e) (lookup e L))
        (else ; compound
         (cond ((eq? (car e) 'add)
                (fold-right + 0 (map (lambda (e) (evaluate e L)) (cdr e))))
               ((eq? (car e) 'sub)
                (fold-right - 0 (map (lambda (e) (evaluate e L)) (cdr e))))
               ((eq? (car e) 'mul)
                (fold-left * 1 (map (lambda (e) (evaluate e L)) (cdr e))))))))

;; Fall 2018

;; #1. Last and all-but-last
(define (last L)
  (if (null? (cdr L))
      (car L)
      (last (cdr L))))

(define (init L)
  (if (null? (cdr L))
      '()
      (cons (car L) (init (cdr L)))))

;; #2. Antitranspose
(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m)
            (transpose (map cdr m)))))

(define (antitranspose m)
  (reverse (map reverse (transpose m))))

;; #3. apply-left and apply-right

(define (apply-left funlist x)
  (apply-right (reverse funlist) x))

(define (apply-right funlist x)
  (if (null? funlist)
      x
      ((car funlist) (apply-right (cdr funlist)
                                  x))))

;; #4. first-atom and last-atom

;; #5. maxdepth
(define (maxdepth L)
  (if (null? L)
      1
      (if (list? (car L))
          (max (+ 1 (maxdepth (car L)))
               (maxdepth (cdr L)))
          (maxdepth (cdr L)))))

;; #6. counter
(define (counter . args)
  (let ((state (if (null? args) 0 (car args))))
    (lambda (m p)
      (cond ((eq? m 'add)
             (set! state (+ state p))
             state)
            ((eq? m 'sub)
             (set! state (- state p))
             state)
            ((eq? m 'mul)
             (set! state (* state p))
             state)))))

;; #7.
;; ((lambda (x) (x x)) (lambda (x) (x x))) -- itsa quine

;; -- itsa quine.

;; Fall 2019

;; #2. include and exclude
(define (include x L)
  (if (null? L)
      (list x)
      (if (eq? (car L) x)
          L
          (cons (car L) (include x (cdr L))))))

(define (exclude x L)
  (if (null? L)
      '()
      (if (eq? (car L) x)
          (cdr L)
          (cons (car L) (exclude x (cdr L))))))

;; # 3. set adt
(define (Set)
  (let ((data '()))
    (lambda (m p)
      (cond ((eq? m 'insert!)
             (set! data (include p data))
             data)
            ((eq? m 'remove!)
             (set! data (exclude p data))
             data)
            ((eq? m 'contains?)
             (if (memq p data) #t #f)))))) ; memq returns a truthy value, not necessarily #t

;; # 4. split
(define (split L)
  (define (spliter s n r l)
    (if (null? l)
        (list (reverse s) (reverse n) (reverse r))
        (cond ((symbol? (car l))
               (spliter (cons (car l) s)
                        n
                        r
                        (cdr l)))
              ((number? (car l))
               (spliter s
                        (cons (car l) n)
                        r
                        (cdr l)))
              (else
               (spliter s
                        n
                        (cons (car l) r)
                        (cdr l))))))
  (spliter '() '() '() L))

;; # 5. multimap
(define (multimap f l)
  (map (lambda (x) (apply f x)) (apply zip L)))        

