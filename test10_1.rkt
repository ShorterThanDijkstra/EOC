#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")
(require "interp-Lcast.rkt")

(define p0 (read-program "./tests/gradual_test_0.rkt"))

