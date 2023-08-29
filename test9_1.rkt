#lang racket
(require graph)
(require "utilities.rkt")
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")
(require "type-check-Lany.rkt")
(require "type-check-Cany.rkt")

(define p0 (read-program "./tests/dynamic_test_4.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (reveal-functions p2))

(define p4 (cast-insert p3))

; (define p5 (type-check-Lany p4))

(define p5 (reveal-casts p4))

(define p6 (convert-assignment p5))

(define p7 (convert-to-closures p6))

(define p8 (limit-functions p7))

(define p9 (expose-allocation (type-check-Lany p8)))

(define p10 (uncover-get! p9))

(define p11 (remove-complex-opera* p10))

(define p12 (explicate-control p11))

(define p13 (select-instructions (type-check-Cany p12))) ;;; type checking fails, hard to debug

(define p14 (uncover-live p13))

(define p15 (build-interference p14))

(define p16 (allocate-registers p15))

(define p17 (patch-instructions p16))

(define p18 (prelude-and-conclusion p17))
