(define (id [x : Integer]) : Integer x)

(let ([x (read)])
  (if (>= x 10)
      (id 42)
      12))
