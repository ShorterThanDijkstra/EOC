#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Cvec.rkt")

(define p0 (read-program "./tests/eco_test_19.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (type-check-Lvec p2))

(define p4 (expose-allocation p3))

(define p5 (uncover-get! p4))

(define p6 (remove-complex-opera* p5))

(define p7 (explicate-control p6))

(define p8 (type-check-Cvec p7))

(define p9 (select-instructions p8))

(define p10 (uncover-live p9))

(define p11 (build-interference p10))

(define p12 (allocate-registers p11))

(define p13 (patch-instructions p12))

(define p14 (prelude-and-conclusion p13))