#lang racket
(define g '((1 2) (2 3) (3 1 4) (4 2)))

(define (succs node)
  (if  (null? (assoc node g))
      '()
      (rest (assoc node g))))

(define (cycle? p)
      (cond
        [(null? p) #f]
        [(= (length p) 1) #f]
        [(equal? (first p) (find-last-el p)) #t]
        [else #f]))

(define (find-last-el xs)
  (cond
    [(null? xs) #f]
    [(null? (rest xs)) (first xs)]
    [else (find-last-el (rest xs))]))

;(max-cycle '((1 2) (2 3) (3 1 4) (4 2)) 1)


(define (gen-next path)
    (map (lambda (x) (cons x path)) (succs (car path))))

(define (generate-paths xs)
  (apply append (map gen-next xs)))

(define (curry n f)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((curry (- n 1) f) x)))))

(define (find-cycles xs len)
   (filter (λ (x) (cycle? x))
           ((curry len generate-paths) xs)))

(define (max-cycle g x)
  (define (helper len)
    (if (null? (find-cycles (nodes g) len))
        (helper (- len 1))
        (flatten (map (lambda (x) (reverse x))
                (filter (lambda (y) (equal? x (first y)))
                (max-length-lst (find-cycles (nodes g) len)))))))
  (helper (len-of-nodes g)))

(define (len-of-nodes g)
  (length (remove-duplicates (flatten g))))

(define (nodes g)
  (map (lambda (x) (list x)) (remove-duplicates (flatten g))))

(define (max-length-lst xs)
  (filter (λ (x) (equal? (length x)
  (apply max (map (λ (x) (length x)) xs)))) xs))