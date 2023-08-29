(let ([x 10])
 (if (or (or (< x 10) (not #f))
         (and (and #t (eq? x 10))
              #f))
    137
    779))
