#lang racket
(define (count-occurences xs sxs)
    (length (filter (λ (x) (equal? x sxs)) (l-combs (length sxs) xs))))

(define (l-combs len xs)
  (define (helper hxs combs xs)
    (cond
      [(null? xs) (append combs (list hxs))]
      [(= len (length hxs)) (helper '() (append combs (list hxs))  xs)]
      [(helper (append hxs (list (first xs))) combs (rest xs))]))
  (helper '() '() xs))

(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (first tree))

(define (left-tree tree)
  +(second tree))

(define (right-tree tree)
  (third tree))

(define (leaf? tree)
  (and (null? (second tree)) (null? (third tree))))

(define tree (make-tree 1 '(2 (3 (4 () ()) ()) ()) '(2 (5 (6 () ()) ()) ())))

(define (sum-tree tree)
  (if (equal? '() tree)
      0
      (+ (root tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree)))))

(define (tree2list tree)
    (if (null? tree)
        '()
        (append (tree2list (left-tree tree)) (list (root tree)) (tree2list (right-tree tree)))))

(define (level-k tree k)
  (define (helper tree count)
    (if (null? tree)
         '()
         (if (= k count)
             (list (root tree))
             (append (helper (left-tree tree) (+ 1 count)) (helper (right-tree tree) (+ 1 count))))))
  (helper tree 0))
            

(define (depth tree)
  (define (helper depth tree)
    (cond
      [(null? tree) depth]
      [(leaf? tree) (+ 1 depth)]
      [else (max (helper (+ 1 depth) (left-tree tree)) (helper (+ 1 depth) (right-tree tree)))]))
  (helper 0 tree))

(define (bfs tree)
  (apply append (map (λ (x) (level-k tree x)) (range 0 (depth tree)))))
        
(define (max-elem tree)
  (apply max (tree2list tree)))

(define (dedup xs)
  (define (partial2 f x)
    (λ (y) (f x y)))
  
  (define (diff? x y)
    (not (equal? x y)))
  
  (if (null? xs)
      '()
      (cons (first xs) (dedup (filter (partial2 diff? (first xs)) xs))))) 

