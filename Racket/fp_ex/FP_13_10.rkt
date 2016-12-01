#lang racket
; find number of digits in a number
; recursion
(define (digits n)
  (if (< n 10) 1
               (+ 1 (digits (quotient n 10)))))
; second variant - iterative ( BETTER! )
(define (digitCount n)
  (define (addDigit n x)
    (if (< n 10)
        (+ x 1)
        (addDigit(quotient n 10) (+ 1 x))))

    (addDigit n 0))

;sum of digits
(define (sumOfDigits n)
  (if (< n 10)
      n
      (+ (sumOfDigits (quotient n 10)) (remainder n 10))))

;number to power
(define (power n a)
  (define (helper n a x)
    (if (= x 0)
        1
        (if (> x 0)
            (* n (helper n a (- x 1)))
            (* (/ 1 n) (helper n a (+ x 1))))))
  (helper n a a))

; reverse number TODO finish
(define (reverse n)
  (define (reverser n a)
    (if (< n 10)
        n
        (+ (* (expt 10 (- a 1))
                       (remainder n 10)) (reverser (quotient n 10)
                    (- a 1)
                    ))))
    (reverser n (digits n)))

;prime
(define (prime? n)
  (define (helper n a)
    (cond
      [(= n 1) #f]
      [(= a n) #t]
      [(= 0 (remainder n a)) #f]
      [else (helper n (+ 1 a))]))
  (helper n 2))
  
      
        
        
        
        
      
  