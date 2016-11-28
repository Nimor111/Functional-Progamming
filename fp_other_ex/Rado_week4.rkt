#lang racket
; finds sum of numbers ( numbers is a list )
(define (sum numbers)
  (if (null? numbers)
      0
      (+ (first numbers) (sum (rest numbers)))))

; finds sum of numbers iteratively
(define (sum-iter numbers)
  (define (helper dst numbers)
    (if (null? numbers)
        dst
        (helper (+ dst (first numbers)) (rest numbers))))
  (helper 0 numbers))

; is element a member of list
(define (member? x lst)
  (cond
    [(null? lst) #f]
    [(= (first lst) x) #t]
    [else (member? x (rest lst))]))

; finds length of list
(define (length2 lst)
  (define (helper lst count)
    (if (null? lst)
        count
        (helper (rest lst) (+ 1 count))))
  (helper lst 0))

; finds n-th member of list
(define (list-ref2 lst idx)
  (if (> idx (length lst))
      (error "Out of bounds")
      (if (= idx 0)
          (first lst)
          (list-ref2 (rest lst) (- idx 1)))))

;makes list from range
(define (range2 a b)
  (if (= a (- b 1))
      (cons a '())
      (cons a (range2 (+ 1 a) b))))

;makes list with (f i) for i from 0 to n
(define (build-list2 n f)
  (define (helper el)
    (if (= el (- n 1))
        (cons (f (- n 1)) '())
        (cons (f el) (helper (+ 1 el)))))
  (helper 0))

;takes first n els of a list
(define (take2 lst n)
  (if (> n (length lst))
      lst
      (if (= n 1)
          (cons (first lst) '())
          (cons (first lst) (take2 (rest lst) (- n 1))))))
     