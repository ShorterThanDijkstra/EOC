; #lang racket
(define (id [x : Integer]) : Integer x)

(let ([y (if #t
             (read)
             (if (eq? (read) 0)
                 777
                 (let ([x (read)])
                   (+ 1 x))))])
  (id (- y 57)))