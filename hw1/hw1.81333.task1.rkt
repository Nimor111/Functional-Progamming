#lang racket
(define (prime? n)
  (define (helper n a)
    (cond
      [(= n 1) #f]
      [(= a n) #t]
      [(= 0 (remainder n a)) #f]
      [else (helper n (+ a 1))]))
  (helper n 2))

(define (sum_of_primes n k)
  (define (helper n a count res)
    (cond
      [(= count n) res]
      [(equal? (prime? a) #t) (helper n (+ 1 a) (+ 1 count) (+ res a))]
      [else (helper n (+ 1 a) count res)]))
  (helper n (+ 1 k) 0 0))
    

    
        
        
        
    