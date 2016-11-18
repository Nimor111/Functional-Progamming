#lang racket
; VARIANT 1


; truncatable prime
;числото х е просто
;всички числа, които се получават чрез премахване на цифри в края на х също са прости
;Пример за такова число е 3797, тъй като 3797 е просто и числата, които се получават чрез последователно премахване на цифри в края му (379, 37 и 3), също са прости.

(define (prime? x)
 (define (helper nums num)
   (cond
   [(= 1 num) #f]
   [(= 2 num) #t]
   [(= num nums) #t]
   [(= 0 (remainder num nums)) #f]
   [else (helper (+ 1 nums) num)]))
  (helper 2 x))

(define (truncatable-prime x)
  (cond
    [(equal? (prime? x) #f) #f]
    [(and (prime? x) (< x 10)) #t]
    [else (truncatable-prime (quotient x 10))]))

;where - filter all predicates
(define (where list-elements list-predicates)
  (if (null? list-predicates)
      list-elements
      (where (filter (first list-predicates) list-elements) (rest list-predicates))))

(define A (list (list 1 2 3 4) (list 5 0 6 7) (list 8 9 10 0)))

(define (transpose xss)
  (apply map list xss))

;zero - subst cols with zero to be all zeros
(define (zero matrix)
  (define (helper xss new)
    (if (null? xss)
        (transpose new)
        (if (member 0 (first xss))
            (helper (rest xss) (append new (list (map (λ (x) 0) (first xss)))))
            (helper (rest xss) (append new (list (first xss)))))))
  (helper (transpose matrix) '()))

;Magic square
(define (prim-diag xss)
  (define (helper count xss)
    (if (null? xss)
        '()
        (append (list (list-ref (first xss) count)) (helper (+ 1 count) (rest xss)))))
  (helper 0 (transpose xss)))

(define (scd-diag xss)
  (prim-diag (transpose xss)))

; Magic square
(define (magic-square? xss)
  (if (and (equal? (map (λ (x) (apply + x)) xss) (map (λ (x) (apply + x)) (transpose xss)))
           (equal? (apply + (prim-diag xss)) (apply + (scd-diag xss)))
           (equal? (apply + (prim-diag xss)) (first (map (λ (x) (apply + x)) xss))))
      #t
      #f))

;Repeater
(define (repeater str)
  (λ (count glue)
    (define (helper cnt new)
      (if (= count cnt)
          new
          (if (= (- count 1) cnt)
              (helper (+ 1 cnt) (string-append new str))
              (helper (+ 1 cnt) (string-append new str glue)))))
    (helper 0 "")))


; VARIANT 2

; interesting sum

(define (find-denoms x)
  (define (helper count dxs)
    (cond
      [(or (= x 1) (= x 0)) '()]
      [(= count x) dxs]
      [(= 0 (remainder x count)) (helper (+ 1 count) (append dxs (list count)))]
      [else (helper (+ 1 count) dxs)]))
  (helper 1 '()))

(define (dfunc dxs)
  (apply + dxs))

(define (interesting? x)
  (equal? x (dfunc (find-denoms (dfunc (find-denoms x))))))

(define (sum-interesting limit)
  (apply + (filter interesting? (range 1 (+ 1 limit)))))
  
; zero row
(define (zero-row xss)
  (define (helper new xss)
    (if (null? xss)
        new
        (if (member 0 (first xss))
            (helper (append new (list (map (λ (x) 0) (first xss)))) (rest xss))
            (helper (append new (list (first xss))) (rest xss)))))
  (helper '() xss))
     
; permutations
(define (partial2 f x)
  (λ (y) (f x y)))

(define (number-to-list x)
  (if (< x 10)
      (list x)
      (sort (append (number-to-list (quotient x 10)) (list (remainder x 10))) <)))  

(define (all-permutations? items)
  (equal? (map number-to-list items) (filter (partial2 equal? (first (map number-to-list items))) (map number-to-list items))))

; cycle
(define (cycle-xs xs)
  (append (list (first (reverse xs))) (reverse (rest (reverse xs)))))

(define (cycle-times times xs)
  (if (= times 0)
      xs
      (cycle-times (- times 1) (cycle-xs xs))))

(define (get-idx x xs)
  (define (helper count xs)
    (cond
      [(= (first xs) x) count]
      [(helper (+ 1 count) (rest xs))]))
  (helper 0 xs))

(define (cycle times items)
  (λ (x) (get-idx x (cycle-times times items))))

; 3rd prob digits sum
;(define (integers-from n)
;   (stream-cons n (integers-from (+ 1 n))))
;(define integers (integers-from 1))

(define (sum-with-no-zero x)
  (apply + (filter (λ (x) (not (equal? x 0))) (number-to-list x))))

(define (cons-lgst-num x)
  (if (= 1 x)
      (list (integer->char (+ 48 1)))
      (append (list (integer->char 49)) (cons-lgst-num (- x 1)))))

(define (make-number n)
  (string->number (list->string (cons-lgst-num n))))
  
(define (digits-sum n)
  (apply + (filter (λ (x) (and (equal? #f (member 0 (number-to-list x))) (equal? n (sum-with-no-zero x)))) (range 1 (+ 1 (make-number n))))))
