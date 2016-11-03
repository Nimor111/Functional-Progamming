#lang racket
(define (list-of-numbers n)
  (define (helper counter)
  (if (= counter n)
      (list n) ; => (cons n '())
      (cons counter (helper (+ 1 counter)))))
  (helper 1))

(define (member? lst x)
  (if (null? lst)
      #f
      (if (equal? x (first lst))
      #t
      (member? (rest lst) x))))
  
(member? '((1 2 3)) '(1 2 3)) ; #t
(member? '(1 2 3) '(1 2 3)) ; #f

(define (remove lst x)
  (cond
    [(null? lst) '()]
    [(equal? x (first lst)) (rest lst)]
    [else (cons (first lst) (remove (rest lst) x))]))

(define (get-i i lst)
  (define (helper counter lst)
    (if (= counter i)
        (first lst)
        (helper (+ 1 counter) (rest lst))))
  (helper 0 lst))

(define (get-i2 i lst)
  (if (= i 0)
      (first lst)
      (get-i (- i 1) (rest lst))))

;(define (remove-i i lst)
 ; (remove lst (get-i i lst)))

(define (remove-i i lst)
  (if (= i 0)
      (rest lst)
      (cons (first lst) (remove-i (- i 1) (rest lst)))))
    
(define (map func lst)
  (if (null? lst)
      '()
      (cons (func (first lst)) (map func (rest lst)))))

(define (filter func lst)
  (if (null? lst)
      '()
      (if (equal? #t (func (first lst)))
          (cons (first lst) (filter func (rest lst)))
          (filter func (rest lst)))))

;(define (pcons a b)
 ; (λ (v) (if (= v 1) a b)))

;(define (pcar p)
 ; (p 1))

;(define (pcdr p)
 ; (p 2))

;(define p (pcons 1 2))

;(define lst (pcons 1 (pcons 2 '())))

;(pcar p) -> 1
;(pcdr p) -> 2

(define (length lst) 
  (if (null? lst)
      0
      (+ 1 (length(rest lst)))))

(define (length-iter lst)
  (define (helper count lst)
    (if (null? lst)
        count
        (helper (+ 1 count) (rest lst))))
  (helper 0 lst))

(define (f1 lst)
  (if (null? lst)
      '()
      (cons (length (first lst)) (f1 (rest lst)))))

(define (f1-2 lst)
  (map length lst))