#lang racket
(define (digits n)
  (define (helper n a)
    (if (< n 10)
        (+ 1 a)
        (helper (quotient n 10) (+ a 1))))
  (helper n 0))

(define (reverse num)
  (define (helper num count)
    (if (< num 10)
        num
        (+ (* (expt 10 (- count 1)) (remainder num 10))
           (helper (quotient num 10) (- count 1)))))
  (helper num (digits num)))

(define (palindrome? n)
  (if (= n (reverse n))
      #t
      #f))

(define (check-palindromes a b)
  (define (helper a b count)
    (cond
      [(> a b) count]
      [(if (equal? (palindrome? a) #t)
           (helper (+ 1 a) b (+ 1 count))
           (helper (+ 1 a) b count))]))
  (helper a b 0))