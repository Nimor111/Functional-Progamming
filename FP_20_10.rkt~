#lang racket
; tail recursion
(define (gcd n k)
  (cond
    [(= n k) n]
    [(> n k) (gcd (- n k) k)]
    [else (gcd n (- k n))]))

;fact helper
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; sin with tailor's set
(define (sine x n)
    (if (= n 0)
        x
        (+ (/ (* (expt -1 (- 1 n)) (expt x (- (* n 2) 1)))
              (fact (- (* n 2) 1))) (sine x (- n 1)))))

;(define (sine x n)
 ; (define (next prev i x)
  ;  (/ (*-1 x x prev) (* i ( + i 1)))) 

; cosh
(define (cosh x n)
  (define (acc x i prev next next-i n)
    (if (> i n)
      0
      (+ prev (acc x (next-i i) (next prev i x) next next-i n))))
  (define (next prev i x)
    (/ (* x x prev) (+ 1 (* 2 i)) (+ 2 (* 2 i))))
  (acc x 0 1 next (λ (i) (+ i 1)) n))

; examples with λ expressions
(λ (x) (+ 2 1 x))
((λ (x) (+ 2 1 x)) 6)