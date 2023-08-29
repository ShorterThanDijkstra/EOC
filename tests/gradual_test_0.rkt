(define (map [f : (Integer -> Integer)]
             [v : (Vector Integer Integer)]) : (Vector Integer Integer)
        (vector (f (vector-ref v 0)) (f (vector-ref v 1))))

(define (inc x) (+ x 1))

(define (true) #t)

(define (maybe_inc x) (if (eq? 0 (read)) (inc x) (true)))

(vector-ref (map maybe_inc (vector 0 41)) 0)