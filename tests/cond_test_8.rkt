(let ([x #t])
  (if x (let ([y 12]) (+ y 13)) (if #t #f #t)))
