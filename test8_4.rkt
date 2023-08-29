#lang racket
(require "utilities.rkt")
(require "compiler.rkt")
(require "type-check-Llambda.rkt")
(require "type-check-Clambda.rkt")

(define p0 (read-program "./tests/lambda_test_1.rkt"))

(define p1 (shrink p0))

(define p2 (uniquify p1))

(define p3 (reveal-functions p2))

(define p4 (convert-assignment p3))

(define p5 (convert-to-closures p4))

(define p6 (limit-functions p5))

(define p7 (type-check-Llambda p6))

(define p8 (expose-allocation p7))

(define p9 (uncover-get! p8))

(define p10 (remove-complex-opera* p9))

(define p11 (explicate-control p10))

(define p12 (type-check-Clambda p11))

(define p13 (select-instructions p12))

(define p14 (uncover-live p13))

(define p15 (build-interference p14))

(define p16 (allocate-registers p15))

(define p17 (patch-instructions p16))

(define p18 (prelude-and-conclusion p17))
