(define (id [x : Integer]) : Integer x)

(let ([x (if (< 11 (read))
             13
             14)])
  (let ([y (+ 11 x)])
    (let ([z (+ 13 y)])
      (id (+ z 5)))))