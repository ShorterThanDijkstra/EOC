(let ([x (let ([x 17]) (+ x 9))])
  (let ([y (let ([z 13]) (+ z (read)))]) (+ x y)))