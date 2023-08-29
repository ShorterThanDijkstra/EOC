#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")
(require "type-check-Lfun.rkt")
(require "type-check-Cfun.rkt")
(require "interp-Lfun.rkt")

(define p0 (read-program "./tests/eco_test_22.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (reveal-functions p2))

(define p4 (limit-functions p3))

(define p5 (type-check-Lfun p4))

(define p6 (expose-allocation p5))

(define p7 (uncover-get! p6))  

(define p8 (remove-complex-opera* p7))

(define p9 (explicate-control p8)) 

(define p10 (type-check-Cfun p9))

(define p11 (select-instructions p10))

(define p12 (uncover-live p11))

(define p13 (build-interference p12))

(define p14 (allocate-registers p13))

(define p15 (patch-instructions p14))

(define p16 (prelude-and-conclusion p15))

