; #lang racket
(define (id [x : Integer]) : Integer x)

(let ([x (eq? 1 3)])
  (if x
      (id 13)
      42))