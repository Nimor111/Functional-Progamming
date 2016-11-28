#lang racket
(define (sum-digits x)
  (if (< x 10)
      x
      (+ (remainder x 10) (sum-digits (quotient x 10)))))

; dumb solution
(define (sum-sum-digit a b k)
  (define (helper count sum)
    (cond
      [(and (= count b) (not (= (remainder (sum-digits count) k) 0))) sum]
      [(and (= count b) (= (remainder (sum-digits count) k) 0)) (+ count sum)]
      [(= (remainder (sum-digits count) k) 0) (helper (+ 1 count) (+ sum count))]
      [else (helper (+ 1 count) sum)]))
  (helper a 0))

; legit solution
(define (sum-sum-digit2 a b k)
  (apply + (filter (λ (x) (= (remainder (sum-digits x) k) 0)) (range a (+ 1 b)))))


(define (isinc? xs)
  (cond
    [(null? xs) #t]
    [(null? (rest xs)) #t]
    [(>= (first xs) (second xs)) #f]
    [else (isinc? (rest xs))]))

(define (find-last-el xs)
  (cond
    [(null? xs) '()]
    [(= (length xs) 1) (first xs)]
    [else (find-last-el (rest xs))]))

(define (append-el x xs)
  (if (null? xs)
      (list x)
      (append xs (list x))))

(define (map-increasing-sublists xs)
  (define (helper max-xss curxs xsh)
    (cond
      [(null? xsh) (append max-xss (list curxs))]
      [(null? curxs) (helper max-xss (append-el (first xsh) curxs) (rest xsh))]
      [(and (isinc? curxs) (> (first xsh) (find-last-el curxs))) (helper max-xss (append-el (first xsh) curxs) (rest xsh))]
      [else (helper (append max-xss (list curxs)) '() xsh)]))
  (helper '() '() xs))

(define (max-increasing-sublist xs)
  (first (filter (λ (x) (= (length x) (apply max (map (λ (x) (length x)) (map-increasing-sublists xs))))) (map-increasing-sublists xs))))

(define A  '((-1 3 4) (0 1 2) (0 0 5)))

(define (triangular? mxs)
  (cond
    [(null? mxs) #t]
    [(not (= (first (map (λ (row) (first row)) mxs)) (apply + (map (λ (row) (first row)) mxs)))) #f]
    [else (triangular? (rest (map (λ (row) (rest row)) mxs)))]))

;(= (first (map (λ (row) (first row)) mxs)) (apply + (map (λ (row) (first row)) mxs)))

(define (pc fs x)
  (cond
    [(null? fs) 0]
    [(null? (rest fs)) x]
    [else (+ ((first fs) ((second fs) x)) (pc (rest (rest fs)) x))]))

(define (pair-compose fs)
  (λ (x) (pc fs x)))



      
