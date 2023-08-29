(define (id [x : Integer]) : Integer x)

(let ([x (if (< 0 (read)) 10 11)])
  (+ x (id 32)))