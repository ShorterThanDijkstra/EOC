(define (id [x : Integer]) : Integer x)

(let ([x (read)])
 (begin (while (< x 3)
    (set! x (read)))
   (id x)))