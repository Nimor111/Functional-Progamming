#lang racket
(define (divisors n)
  (define (helper n a count)
    (cond
      [(= a n) (+ 1 count)]
      [(= 0 (remainder n a)) (helper n (+ 1 a) (+ 1 count))]
      [else (helper n (+ 1 a) count)]))
  (helper n 1 0))