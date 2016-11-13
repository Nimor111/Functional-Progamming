#lang racket
; Variadic function ( with variable number of args )
 (define (f . xs)
    (if (null? xs)
        (error "No args given")
        (map (λ (x) (* x 2)) xs)))

;( f 1 2 3 ) -> ( 2 4 6)
; (f 1 2 3 4) -> ( 2 4 6 8)
