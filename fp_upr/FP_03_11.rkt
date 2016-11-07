#lang racket
(define (minelem lst)
  (if (= (length lst) 1)
      (first lst)
      (min (first lst) (minelem (rest lst)))))

  (define (helper lst x)
      (if (= 1 (length lst))
          ((first lst) x)
          ((first lst) (helper (rest lst) x))))

(define (composition lst)
  (λ (x) (helper lst x)))
  
((composition (list (λ (x) (+ x 1)) (λ (x) (* x x)))) 1)

(define (sq-of-odds lst)
  (apply + (map sqr (filter odd? lst))))

(sq-of-odds '(1 2 3))

(define (add-to-start lst x)
  (map (λ (m) (cons x m)) lst))

(add-to-start '((1 2) (3 4)) 1)

(define (member? el lst)
  (cond
    [(null? lst) #f]
    [(= (first lst) el) #t]
    [else (member? el (rest lst))]))
      

(define (intersection A B)
  (cond
    [(null? A) '()]
    [(null? B) '()]
    [(equal? #t (member? (first A) B))
     (cons (first A) (intersection (rest A) B))]))

(define (intersection2 A B)
  (filter (λ (x) (member? x B)) A))

(intersection2 '(1 2 3 4 5) '(1 2 3 4))

(define (union A B)
  (append A (filter (λ (x) (not (member? x A))) B)))

(define (countmin lst)
  (define (helper (count lst))
    (if (= 1 (length lst))
        count
        
        
                  
