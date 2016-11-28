#lang racket
; partial func with 1 arg that returns func of another arg
; can be called with two args
; currying
(define (partial2 f a)
    (λ (b) (f a b)))

; composition h(x) = f(g(x))
; works only with one arg
(define (compose f g)
  (λ (x)
    (f (g x))))

;week6 probs

;is list an increasing sequence
(define (increasing? ns)
  (cond
    [(null? ns) #t]
    [(null? (rest ns)) #t]
    [(>= (first ns) (second ns)) #f]
    [else (increasing? (rest ns))]))


;is list a decreasing sequence
(define (decreasing? ns)
  (cond
    [(null? ns) #t]
    [(null? (rest ns)) #t]
    [(< (first ns) (second ns)) #f]
    [else (decreasing? (rest ns))]))

; occurences of y in xs
(define (count y xs)
  (cond
    [(null? xs) 0]
    [(equal? y (first xs)) (+ 1 (count y (rest xs)))]
    [else (count y (rest xs))]))

;second sol
(define (count2 y xs)
  (apply + (map (lambda (x) (if (equal? x y)
                                1
                                0)))))

;third sol
(define (count3 y xs)
  (length (filter (λ (x) (equal? x y)) xs)))

;remove equal elements in list
(define (dedup xs)
  (if (null? xs)
      '()
      (cons (first xs) (dedup (filter (partial2 equal? (first xs)) xs)))))

(define (append-el x lst)
  (if (null? lst)
      (cons x '())
      (append  lst (list x))))

(define (member? x xs)
  (cond
    [(null? xs) #f]
    [(equal? x (first xs)) #t]
    [else (member? x (rest xs))]))
      

(define (dedup2 xs)
  (define (helper xs2 lst)
    (cond
      [(null? lst) xs2]
      [(member? (first lst) xs2) (helper xs2 (rest lst))]
      [else (helper (append-el (first lst) xs2) (rest lst))]))
  (helper '() xs))
  
;sum all els of a matrix
(define (sum-matrix mns)
  (apply + (map (λ (row) (apply + row)) mns)))

(define A (list (list 1 2 3)
                  (list 4 5 6)
                  (list 7 8 9)))

;get element at index
(define (get-element x y matrix)
  (list-ref (map (λ (row) (list-ref row x)) matrix) y))

; get row of matrix
(define (get-row n matrix)
  (list-ref (map (λ (row) row) matrix) n))

;get column of matrix
(define (get-col n matrix)
  (map (λ (row) (list-ref row n)) matrix))

(define (get-index idx lst)
  (define (helper count lst)
    (cond
      [(= 1 (length lst)) (first lst)]
      [(= count idx) (first lst)]
      [else (helper (+ 1 count) (rest lst))]))
  (helper 0 lst))

;get diagonal
(define (main-diagonal matrix)
  (define (helper count diag)
    (cond
      [(= count (length matrix)) diag]
      [(helper (+ 1 count) (append-el (get-index count (get-col count matrix)) diag))]))
  (helper 0 '()))
  