#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")

(define p0 (read-program "./tests/eco_test_1.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (remove-complex-opera* p2))

(define p4 (explicate-control p3))

(define p5 (select-instructions p4))

(define p6 (uncover-live p5))

(define p7 (build-interference p6))

(define p8 (allocate-registers p7))

(define p9 (remove-jumps p8))

(define p10 (patch-instructions p9))
