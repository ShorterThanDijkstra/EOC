(let ([x 10])
  (if (and (or (< x 10) (>= x 7))
           (not #t))
      999
      789))
