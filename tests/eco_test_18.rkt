; #lang racket
; (let ([v (vector (vector 44))])
; (let ([x (let ([w (vector 42)])
; (let ([_ (vector-set! v 0 w)])
; 0))])
; (+ x (vector-ref (vector-ref v 0) 0))))
(define (id [x : Integer]) : Integer x)

42