; #lang racket
(define (fib a b i n)
  (if (eq? i n)
      b
      (fib b (+ a b) (+ i 1) n)))

(fib 0 1 0 10)