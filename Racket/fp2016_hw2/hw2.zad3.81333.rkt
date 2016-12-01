#lang racket

(define (max-abs-lst lst x)
  (if (= 1 (length lst))
      (abs ((first lst) x))
      (max (abs ((first lst) x)) (max-abs-lst (rest lst) x))))

(define (return-func-val func-lst x)
  (if (equal? (abs ((first func-lst) x)) (max-abs-lst func-lst x))
      ((first func-lst) x)
      (return-func-val (rest func-lst) x)))

(define (maximize func-lst)
  (lambda (x) (return-func-val func-lst x)))

((maximize (list (λ (x) (- x 10)))) 5)
((maximize (list (λ (x) (- x 10)) (λ (x) (- x 5)))) 5) ;-> -5
((maximize (list (λ (x) (- x 10)) (λ (x) (- x 5)))) 9) ;-> 4
((maximize (list (λ (x) (expt x 2)) (λ (x) (expt x 3)) (λ (x) (expt x 4)))) 2)