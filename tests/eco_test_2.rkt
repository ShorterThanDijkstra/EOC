(define (id [x : Integer]) : Integer x)

(let ([a (let ([b 13])
           (let ([c (let ([d 17])
                  (+ d 19))])
             (let ([e 11])
         (+ e 13))))])
    (+ (id 25) 17))
