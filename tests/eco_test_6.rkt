(define (id [x : Integer]) : Integer x)

(let ([x (read)])
  (let ([y (read)])
    (if (eq? x 2)
        (if (eq? y 2) (+ y 11) (+ y x))
        (id (+ y 10)))))
