#lang racket
(define (convert-to-dig n)
  (- (char->integer n) 48))

(define (ascii string)
  (map (λ (x) (convert-to-dig x)) (string->list string)))

(define (get-numbers lst)
    (cond
      [(null? lst) '()]
      [(> (first lst) 10) (cons 0 (get-numbers (rest lst)))]
      [else (cons (first lst) (get-numbers (rest lst)))]))

(define (convert-to-char n)
  (if (equal? 0 n)
      -1
      (integer->char (+ 48 n))))

(define (convert-to-chars lst)
  (map (λ (x) (convert-to-char x))  lst))

(define (convert-to-num lst)
  (string->number (list->string lst)))

(define (append-el el lst)
  (append lst (cons el '())))

(define (split-by lst)
  (define (helper l new res)
    (cond
      [(null? l) (append new (list res))]
      [(char? (first l)) (helper (rest l) new (append-el (first l) res))]
      [else (helper (rest l) (append new (list res)) '())]))
  (helper lst '() '()))
    
(define (filter-nulls lst)
  (filter (λ (x) (not (null? x))) lst))

(define (sum-numbers string)
  (apply + (map convert-to-num (filter-nulls (split-by (convert-to-chars (get-numbers (ascii string))))))))

(ascii "abc123bc567")
(get-numbers (ascii "abc123bc567"))
(convert-to-chars (get-numbers (ascii "abc123bc567")))
(split-by (convert-to-chars (get-numbers (ascii "abc123bc567"))))
(filter-nulls (split-by (convert-to-chars (get-numbers (ascii "abc123bc567")))))
(map convert-to-num (filter-nulls (split-by (convert-to-chars (get-numbers (ascii "abc123bc567"))))))
(sum-numbers "abc123bc567")