; #lang racket
(define (id [x : Integer]) : Integer x)

(let ([y (let ([x1 20])
           (let ([x2 22])
             (+ x1 x2)))])
  (id 42))