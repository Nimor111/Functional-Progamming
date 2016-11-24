#lang racket
; interface for tree
(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree))

(define (root tree)
  (first tree))

(define (left-tree tree)
  (second tree))

(define (right-tree tree)
  (third tree))

(define (leaf? tree)
  (and (null? (left-tree tree)) (null? (right-tree tree))))

;interface for tree

(define (level-k tree k)
  (define (helper tree count)
    (if (null? tree)
         '()
         (if (= k count)
             (list (root tree))
             (append (helper (left-tree tree) (+ 1 count)) (helper (right-tree tree) (+ 1 count))))))
  (helper tree 0))

(define (is-edge xs tree)
  (cond
    [(null? tree) #f]
    [(and (not (null? (left-tree tree))) (equal? (first xs) (root tree)) (equal? (second xs) (root (left-tree tree)))) #t]
    [(and (not (null? (right-tree tree))) (equal? (first xs) (root tree)) (equal? (second xs) (root (right-tree tree)))) #t]
    [else (or (is-edge xs (right-tree tree)) (is-edge xs (left-tree tree)))]))

(define (edges-on-level tree k)
  (if (= 0 k)
      '()
      (filter (lambda (x) (is-edge x tree)) (cartesian-product (level-k tree (- k 1)) (level-k tree k)))))

    
(define tree '(1 (2 (4 () ()) (5 () (7 () ()))) (3 (6 (8 () ()) (9 () ())) ())))

; tests
(edges-on-level tree 0)
(edges-on-level tree 1)
(edges-on-level tree 2)
(edges-on-level tree 3)