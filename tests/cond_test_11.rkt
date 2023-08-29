(let ([x (let ([y #t]) (if (not y) 11 (+ 11 1)))])
  (let ([z (+ x 10)])
    (if (< z 10)
        13
        17)))
