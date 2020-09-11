
;; Cody Rivera -- CS 403 Exam 1

;; BST node interface
(define (make-node key l r)
  (list key l r))

(define (get-key n)
  (car n))

(define (get-left n)
  (cadr n))

(define (get-right n)
  (caddr n))

(define (BST)
  (let ((T 'nil))
    (lambda (m . args)
      (cond ((eqv? m 'add!)
             (set! T (add (car args) T))
             T)
            ((eqv? m 'search)
             (search (car args) T))))))

(define (search x T)
  (if (eqv? T 'nil)
      '(nil)
      (let ((key (get-key T)))
        (cond ((= x key) (list x))
              ((< x key) (cons key (search x (get-left T))))
              ((> x key) (cons key (search x (get-right T))))))))

;; Functional "add"
(define (add x T)
  (if (eqv? T 'nil)
      (make-node x 'nil 'nil)
      (let ((key (get-key T)))
        (cond ((= x key) T)
              ((< x key)
               (make-node key
                          (add x (get-left T))
                          (get-right T)))
              ((> x key)
               (make-node key
                          (get-left T)
                          (add x (get-right T))))))))

