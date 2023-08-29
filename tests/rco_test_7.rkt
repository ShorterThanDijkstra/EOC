; #lang racket
(let ([foo (let ([a (let ([b (let ([c (let ([d 9])
                                        (+ d 8))])
                               (let ([e (let ([f 10]) (+ f 11))])
                                 (+ (let ([x 15]) (+ x 16)) e))
                               )])
                      (+ b 6))])
             (+ a 5))])
  (+ foo 4))
