; #lang racket
(define (id [x : Integer]) : Integer x)

(id (let ([x 11])
  (begin
    (read)
    (set! x 17)
    (if (< x 13)
        (read)
        (+ 1 (read)))
    (begin (set! x 42) x)
    (+ 11 13)
    x)))