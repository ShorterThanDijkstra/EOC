#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")
(require "type-check-Llambda.rkt")
(require "type-check-Clambda.rkt")

(define p0 (read-program "./tests/assign_conversion_test_1.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (reveal-functions p2))

(define p4 (convert-assignment p3))
