(define (id [x : Integer]) : Integer x)

(let ([x #t])
  (id (if x
      42
      42)))