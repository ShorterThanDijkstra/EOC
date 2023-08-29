(define (f6 [x7 : Integer]) : (Integer -> Integer)
  (let ([y8 4])
    (lambda: ([z9 : Integer]) : Integer
      (+ x7 (+ y8 z9)))))

(let ([g0 (f6 5)])
  (let ([h1 (f6 3)])
    (+ (g0 11) (h1 15))))