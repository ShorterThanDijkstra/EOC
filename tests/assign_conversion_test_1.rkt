(define (g [x : Integer]) : Integer
(let ([f (lambda: ([a : Integer]) : Integer (+ a x))])
(begin
(set! x 10)
(f 32))))

(let ([x 0])
(let ([y 0])
(let ([z 20])
(let ([f (lambda: ([a : Integer]) : Integer (+ a (+ x z)))])
(begin
(set! x 10)
(set! y 12)
(f y))))))