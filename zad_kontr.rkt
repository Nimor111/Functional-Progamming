#lang racket
; zad1/ 2015 var 1
(define (sum-digits a)
  (if (< a 10)
      a
      (+ (remainder a 10) (sum-digits (quotient a 10)))))

(define (sum-sum-digit a b k)
  (define (helper sum count)
    (cond
      [(and (= count b) (not (= (remainder (sum-digits count) k) 0))) sum]
      [(and (= count b) (= (remainder (sum-digits count) k) 0)) (+ sum count)]
      [(= (remainder (sum-digits count) k) 0)
       (helper (+ sum count) (+ 1 count))]
      [else (helper sum (+ 1 count))]))
  (helper 0 a))

; zad2/ 2015 var 1
(define (append-el el xs)
  (if (null? xs)
      (list el)
      (append xs (list el))))

(define (find-last-el lst)
       (if (= (length lst) 1)
           (first lst)
           (find-last-el (rest lst))))

(define (max-ordered-sublist lst)
  (define (helper max-lsts inclst lsth)
    (cond
        [(null? lsth) (first (filter (λ (x) (= (length x) (apply max (map (λ (x) (length x)) (append max-lsts (list inclst)))))) (append max-lsts (list inclst))))]
        [(null? inclst) (helper max-lsts (append-el (first lsth) inclst) (rest lsth))]
        [(equal? #t (< (find-last-el inclst) (first lsth))) (helper max-lsts (append-el (first lsth) inclst) (rest lsth)) ]
        [else (helper (append max-lsts (list inclst)) '() lsth)]))
  (helper '() '() lst))
; (apply max (map (λ (x) (length x))
;(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) 
(define (isinc? lst)
  (cond
    [(null? lst) #t]
    [(null? (rest lst)) #t]
    [(<= (second lst) (first lst)) #f]
    [else (isinc? (rest lst))]))
    
    


  