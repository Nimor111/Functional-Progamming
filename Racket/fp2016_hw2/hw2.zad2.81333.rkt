#lang racket
(define (convert x k n)
  (cond
    [(= n 10) (convert-to-ten x k)]
    [(= k 10) (make-number (convert-from-ten x n))]
    [else (make-number (convert-from-ten (convert-to-ten x k) n))])) 

; helper function
(define (append-el el lst)
  (append lst (cons el '())))

; returns list
(define (convert-from-ten x n)
  (define (helper res num)
    (if (= num 0)
        '(0)
        (if (= num 1) (reverse (append-el num res))
        (helper (append-el (remainder num n) res) (quotient num n)))))
  (helper '() x))

(define (make-number lst)
  (string->number (list->string (map (λ (x) (integer->char (+ 48 x))) lst))))

(define (make-list-from-number num)
 (define (helper lst number)
    (cond
      [(< number 10) (reverse (append-el number lst))]
      [(helper (append-el (remainder number 10) lst) (quotient number 10))]))
  (helper '() num))

; returns number
(define (convert-to-ten x n)
 (define (helper num base pow)
  (if (= (length num) 1)
      (* (expt base pow) (first num))
      (+ (* (expt base pow) (first num)) (helper (rest num) base (- pow 1)))))
  (helper (make-list-from-number x) n (- (length (make-list-from-number x)) 1)))

 (convert 173 8 10)
 (convert 123 10 2)
 (convert 173 8 2)
 (convert 1111011 2 8)
  