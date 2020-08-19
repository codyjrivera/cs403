;; Exercises directly suggested by Richard Borie
;; http://cs403.cs.ua.edu/fall2020/exercises.htm

;; Solved by Cody Rivera, Fall 2020

;; #1. integer exponent -- n > 0
(define (power m n)
  (cond ((= n 0) 1)
        ((odd? n) (* m (power m (- n 1))))
        (else
         (let ((s (power m (/ n 2))))
           (* s s)))))

;; #2. integer log -- requires `power`
(define (log m q)
  (define (log-help n)
    (if (>= (power m n) q)
        n
        (log-help (+ n 1))))
  (log-help 0))

;; #3. n-choose-k
(define (comb n k)
  (cond ((= n 0) 1)
        ((or (= k 0) (= k n)) 1)
        (else (+ (comb (- n 1) k) 
                 (comb (- n 1) (- k 1))))))

;; #4. insertion sort
(define (insertion_sort L)
  (define (insert L i)
    (cond ((null? L) (list i))
          ((< i (car L)) (cons i L))
          (else (cons (car L) (insert (cdr L) i)))))
  (define (insertion_sort-help L R)
    (if (null? R)
        L
        (insertion_sort-help
         (insert L (car R))
         (cdr R))))
  (insertion_sort-help '() L))

;; #5. selection sort
(define (selection_sort L)
  (define (extract-min L) ; L must be non-empty -- returns (min, list) pair
    (let ((min (fold-left min (car L) L)))
      (cons
       min
       (filter (lambda (x) (not (equal? x min))) L))))
  (if (null? L)
      '()
      (let ((min-pair (extract-min L)))
        (cons (car min-pair) (selection_sort (cdr min-pair))))))

;; #6. merge sort
(define (merge a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else
         (if (<= (car a) (car b))
             (cons (car a) (merge (cdr a) b))
             (cons (car b) (merge a (cdr b)))))))

(define (merge_sort L)
  (let ((n (length L)))
    (if (< n 2)
        L
        (let ((ndiv2 (floor (/ n 2))))
          (merge (merge_sort (list-head L ndiv2)) ; list-head IS NON-STANDARD
                 (merge_sort (list-tail L ndiv2)))))))

;; #7. quick sort
(define (quick_sort-partition L p) ; returns (left, right) partition pair
  (define (partition-help l r X)
    (if (null? X)
        (cons l r)
        (if (<= (car X) p)
            (partition-help (cons (car X) l) r (cdr X))
            (partition-help l (cons (car X) r) (cdr X)))))
  (partition-help '() '() L))

(define (quick_sort-pivot L) ; naive pivot
  (car L))

(define (quick_sort L)
  (let ((n (length L)))
    (if (< n 2)
        L
        (let ((partition (quick_sort-partition
                          L
                          (quick_sort-pivot L))))
          (append (quick_sort (car partition))
                  (quick_sort (cdr partition)))))))

;; Binary tree functions will assume a data structure (cons e (cons l r)), where
;; l and r can be '() (makes one look forward to Data declarations and pattern matching)

(define (bst-node e l r) (cons e (cons l r)))
(define (bst-leaf e) (cons e (cons '() '())))
(define (bst-leaf? n) (and (null? (cadr n)) (null? (cddr n))))

;; #8. binary tree presence
(define (bst-member? x T)
  (if (null? T)
      #f
      (if (= x (car T))
          #t
          (if (< x (car T))
              (bst-member? x (cadr T))
              (bst-member? x (cddr T))))))

;; #9. bst insert -- dups are ignored
(define (bst-insert x T)
  (if (null? T)
      (bst-leaf x)
      (if (= x (car T))
          T
          (if (< x (car T))
              (bst-node (car T) (bst-insert x (cadr T)) (cddr T))
              (bst-node (car T) (cadr T) (bst-insert x (cddr T)))))))

;; #10. bst remove
(define (bst-remove x T)
  (define (tree-inf T) ; T must be non-empty
    (if (null? (cddr T))
        (car T)
        (tree-inf (cddr T))))
  (if (null? T)
      '()
      (if (= x (car T))
          (if (bst-leaf? T)
              '()
              (cond ((null? (cadr T)) (cddr T))
                    ((null? (cddr T)) (cadr T))
                    (else
                     (let ((inf (tree-inf (cadr T)))) ; or sup and cddr
                       (bst-node inf
                                 (bst-remove inf (cadr T))
                                 (cddr T))))))
          (if (< x (car T))
              (bst-node (car T) (bst-remove x (cadr T)) (cddr T))
              (bst-node (car T) (cadr T) (bst-remove x (cddr T)))))))

;; #11. bst traversals
(define (inorder T)
  (if (null? T)
      '()
      (let ((left (inorder (cadr T)))
            (right (inorder (cddr T))))
        (append left (list (car T)) right))))

(define (preorder T)
  (if (null? T)
      '()
      (let ((left (preorder (cadr T)))
            (right (preorder (cddr T))))
        (append (list (car T)) left right))))

(define (postorder T)
  (if (null? T)
      '()
      (let ((left (postorder (cadr T)))
            (right (postorder (cddr T))))
        (append left right (list (car T))))))

;; #12. - Skipped

;; #13. Inverse filter
(define (reject p l)
  (filter (lambda (e) (not (p e))) l))

;; #14. Apply each
(define (applyeach L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (cons ((car L1) (car L2)) (applyeach (cdr L1) (cdr L2)))))

;; #15. For all
(define (forall p l)
  (fold-left (lambda (a b) (and a b)) ; strict and
             #t
             (map p l)))

;; #16. Exists
(define (exists p l)
  (not (forall (lambda (e) (not (p e))) l)))
