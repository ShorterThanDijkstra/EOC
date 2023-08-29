(define (f [a1 : Integer]
           [a2 : Integer]
           [a3 : Boolean]
           [a4 : Integer]
           [a5 : Integer]) : Integer
    (if a3 (+ a1 a2) (+ a4 a5)))

(define (g [a1 : Integer]
           [a2 : Integer]
           [a3 : Boolean]
           [a4 : Integer]
           [a5 : Integer]
           [a6 : Boolean]
           [a7 : Integer]
           [a8 : Integer]
           [f : (Integer Integer Boolean Integer Integer -> Integer)]) : Integer
    (if a3 (+ a1 a2) (f a4 a5 a6 a7 a8)))

(g 32 61 #f 21 21 #t 31 73)



