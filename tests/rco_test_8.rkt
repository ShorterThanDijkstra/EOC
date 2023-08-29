; #lang racket
(let ([a (let ([b 3]) (let ([c 4]) (+ c b)))])
  a)