(define (id [x : Integer]) : Integer x)

(let ([x (read)])
  (let ([y (read)])
    (if (if (< x 1)
            #f
            (eq? x 2))
        (+ y (id 2))
        (- y 1))))
