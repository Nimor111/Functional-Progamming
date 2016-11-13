#lang racket
(define (convert x k n)
  (list k n))

(define (append-el el lst)
  (append lst (cons el '())))

(define (convert-from-ten x n)
  (define (helper res num)
    (if (= num 1) (reverse (append-el num res)))
        (helper (append-el (remainder num n) res) (quotient num n)))
  (helper '() x))

(define (make-number lst)
  (string->number (list->string (map (λ (x) (integer->char (+ 48 x))) lst))))

(define (convert-to-ten x n)
  (define (helper num base pow)
    (
  