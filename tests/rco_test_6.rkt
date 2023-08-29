(let ([foo (let ([a (let ([b (let ([c (let ([d 9])
                                        (+ d 8))])
                               (+ c 7))])
                      (+ b 6))])
             (+ a 5))])
  (+ foo 4))
                               