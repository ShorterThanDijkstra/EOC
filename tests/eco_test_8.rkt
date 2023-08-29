(define (id [x : Integer]) : Integer x)

(if (and (eq? (read) 0) (eq? (read) 1))
(id 0)
(id 42))