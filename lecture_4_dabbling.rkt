#lang racket
(define (nth n lst)
  (if (= n 0)
      (first lst)
      (nth (- n 1) (rest lst))))

(define (append-el lst el)
  (if (null? lst)
      (cons el '())
      (cons (first lst) (append-el (rest lst) el))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

(define (reverse lst)
  (if (null? lst)
      '()
      (append-el (reverse (rest lst)) (first lst))))

(define (atom? a) (not (pair? a)))

(define (count-atoms lst)
  (cond
    [(null? lst) 0]
    [(atom? (first lst)) (+ 1 (count-atoms (rest lst)))]
    [else (+ (count-atoms (first lst)) (count-atoms (rest lst)))]))
