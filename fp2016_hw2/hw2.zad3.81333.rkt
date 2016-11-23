#lang racket

(define ((max-abs-lst lst) x)
  (if (= 1 (length lst))
      (abs ((first lst) x))
      (max (abs ((first lst) x)) ((max-abs-lst (rest lst)) x))))

(define (maximize func-lst)
  (max-abs-lst func-lst))

((maximize (list (λ (x) (- x 10)) (λ (x) (- x 5)))) 5) ;-> 5
((maximize (list (λ (x) (- x 10)) (λ (x) (- x 5)))) 9) ;-> 4
((maximize (list (λ (x) (expt x 2)) (λ (x) (expt x 3)) (λ (x) (expt x 4)))) 2)