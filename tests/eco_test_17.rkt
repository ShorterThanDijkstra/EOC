; #lang racket
(define (id [x : Integer]) : Integer x)

(id (let ([x (read)])
  (let  ([y (while (< x 3)
              (set! x (read)))])
    42)))