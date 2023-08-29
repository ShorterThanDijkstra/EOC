#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")

;;; test remove complex opernads

(define p0 (read-program "./tests/eco_test_10.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (uncover-get! p2))

(define p4 (remove-complex-opera* p3))

(define p5 (explicate-control p4))

(define p6 (select-instructions p5))

(define p7 (uncover-live p6))

(define p8 (build-interference p7))

(define p9 (allocate-registers p8))

(define p10 (remove-jumps p9))

(define p11 (patch-instructions p10))
