#lang racket
(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))

(define G1 '((1 2 3 4) ; от а има ребра към b,c,d
            (2 5 6)   ; може да бъде и ориентиран
            (3 1 4)
            (4 2 3 7)
            (5)       ; връх без наследници
            (6 2 5)
            (7 1)))

(define (vertices g)
  (map car G))

(define (successors v g)
  (let [(xs (assoc v g))]
    (if xs (cdr xs) xs)))

(define (has-edge? u v g)
  (if (member v (successors u g))
      #t
      #f))

(define (add-edge u v g)
  (let [(newg (add-vertex u (add-vertex v g)))]
    (map (lambda (lst) (if (equal? (car lst) u)
                           (append lst (list v))
                           lst)) newg)))

(define (add-vertex v g)
  (if (member v (vertices g))
      g
      (cons (list v) g)))

(define (graph-from-edges lst)
  (if (null? lst)
      '()
      (add-edge (caar lst) (cdar lst)
                (graph-from-edges (cdr lst)))))

(define (graph-from-edges2 lst)
  (foldr (lambda (e g) (add-edge (car e) (cdr e) g)) '() lst))

(define (contains-path? path g)
  (cond
    [(or (null? path) (null? (cdr path))) #t]
    [(has-edge? (car path) (cadr path) g)
     (contains-path? (cdr path) g)]
    [else #f]))

(define (make-pairs path)
  (if (or (null? path) (null? (cdr path)))
      '()
      (cons (cons (car path) (cadr path))
            (make-pairs (cdr path)))))

(define (contains-path?? path g)
  (let [(edges (make-pairs path))]
    (null? (filter (lambda (e) (not (has-edge? (car e) (cdr e) g)))
                edges))))

(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g)) (vertices g)))

(define (extend-path path g)
  (if (null? path)
      (map list (vertices g))
      (map (lambda (v) (append path (list v)))
           (filter (lambda (u) (not (member u path)))
                   (successors (last path) g)))))

(define (edge-list g)
  (define (get-edges lst)
    (map (lambda (v) (cons (car lst) v)) (cdr lst)))
  (apply append (map get-edges g)))

(define (invert g)
  (define (flip p) (cons (cdr p) (car p)))
  (graph-from-edges (map flip (edge-list g))))

(define (bfs v g)
  (define (get-next-level current visited)
    (apply append (map (lambda (u)
                         (filter (lambda (v) (not (member v (append current visited))))
                                 (successors u g))) current)))
  (define (helper current visited)
    (let [(next-level (get-next-level current visited))]
      (if (null? next-level)
          (append visited current)
          (helper next-level (append visited current)))))
  (helper (list v) '()))
