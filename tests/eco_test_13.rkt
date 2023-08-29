(define (id [x : Integer]) : Integer x)

(id (let ([x 10])
  (let ([y 0])
    (+ (+ (begin
            (set! y (read))
            x)
          (begin
            (set! x (read))
            y)); read1 + 10
       x)))) ; read1 + 10 + read2
