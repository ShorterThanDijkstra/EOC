#lang racket
(define base
  (class object%
    (super-new)
    (define/public (a)
      (displayln 'a))
    ))

(define (derived base-class)
  (class base-class
    (inherit a)
    (super-new)
    (define/public (b)
      (a)
      (displayln 'b))))

(define d (new(derived base)))