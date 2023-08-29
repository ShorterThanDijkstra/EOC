#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")

(require "interp.rkt")
(require "compiler.rkt")
; (require "type-check-Lvar.rkt")
; (require "type-check-Cvar.rkt")
; (require "interp-Lvar.rkt")
; (require "interp-Cvar.rkt")
; (require "type-check-Lif.rkt")
; (require "interp-Lif.rkt")
; (require "interp-Cif.rkt")
; (require "type-check-Cif.rkt")
; (require "type-check-Lwhile.rkt")
; (require "type-check-Cwhile.rkt")
; (require "interp-Lwhile.rkt")
; (require "interp-Cwhile.rkt")
; (require "type-check-Lfun.rkt")
; (require "type-check-Cfun.rkt")
; (require "interp-Lfun.rkt")
; (require "interp-Cfun.rkt")

(require "type-check-Lany.rkt")
(require "type-check-Cany.rkt")
(require "interp-Lany.rkt")
(require "interp-Cany.rkt")

; (debug-level 1)
; (AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

; (define passes
;   (list (list "uniquify" uniquify interp-Lvar type-check-Lvar)
;         (list "remove-complex" remove-complex-opera* interp-Lvar type-check-Lvar)
;         (list "explicate control" explicate-control interp-Cvar type-check-Cvar)
;         (list "instruction selection" select-instructions interp-pseudo-x86-0)
;         (list "assign homes" assign-homes interp-x86-0)
;         (list "patch instructions" patch-instructions interp-x86-0)
;         (list "prelude and conclusion" prelude-and-conclusion interp-x86-0)
;         ))

; (define passes
;   (list (list "uniquify" uniquify interp-Lvar type-check-Lvar)
;         (list "remove-complex" remove-complex-opera* interp-Lvar type-check-Lvar)
;         (list "explicate control" explicate-control interp-Cvar type-check-Cvar)
;         (list "instruction selection" select-instructions interp-pseudo-x86-0)
;         (list "uncover live" uncover-live interp-pseudo-x86-0)
;         (list "build interference" build-interference interp-pseudo-x86-0)
;         (list "allocate registers" allocate-registers interp-x86-0)
;         (list "patch instructions" patch-instructions interp-x86-0)
;         (list "prelude and conclusion" prelude-and-conclusion interp-x86-0)
;         ))

; (debug-level 1)
; (interp-tests "var" #f passes interp-Lvar "var_test" (tests-for "var"))
; (interp-tests "int" #f passes interp-Lvar "int_test" (tests-for "int"))
; (interp-tests "var" #f passes interp-Lvar "var_test" (tests-for "var"))
; (interp-tests "rco" #f passes interp-Lvar "rco_test" (tests-for "rco"))
; (interp-tests "eco" #f passes interp-Lvar "eco_test" (tests-for "eco"))
; (interp-tests "insel" #f passes interp-Lvar "insel_test" (tests-for "insel"))
; (interp-tests "homes" #f passes interp-Lvar "homes_test" (tests-for "homes"))
; (interp-tests "patch" #f passes interp-Lvar "patch_test" (tests-for "patch"))
; (interp-tests "precon" #f passes interp-Lvar "precon_test" (tests-for "precon"))

(define passes
  (list
   (list "shrink" shrink interp-Lany type-check-Lany)
   (list "uniquify" uniquify interp-Lany type-check-Lany)
   (list "reveal functions" reveal-functions interp-Lany type-check-Lany)
   (list "cast insert" cast-insert interp-Lany type-check-Lany)
   (list "reveal cast" reveal-casts interp-Lany type-check-Lany)
   (list "convert assignment" convert-assignment interp-Lany type-check-Lany)
   (list "convert to closures" convert-to-closures interp-Lany type-check-Lany)
   (list "limit functions" limit-functions interp-Lany type-check-Lany)
   (list "expose allocation" expose-allocation interp-Lany type-check-Lany)
   (list "uncover get!" uncover-get! interp-Lany type-check-Lany)
   (list "remove complex" remove-complex-opera* interp-Lany type-check-Lany)
   (list "explicate control" explicate-control interp-Cany type-check-Cany)
   (list "select instructions" select-instructions interp-pseudo-x86-1)
   (list "uncover live" uncover-live interp-pseudo-x86-1)
   (list "build interference" build-interference interp-pseudo-x86-1)
   (list "allocate registers" allocate-registers interp-pseudo-x86-1)
   (list "patch instructions" patch-instructions interp-x86-1)
   (list "prelude and conclusion" prelude-and-conclusion interp-x86-1)
   ))

; (interp-tests "functions" type-check-Lfun passes interp-Lfun "lambda_test" (tests-for "lambda"))


;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
; (compiler-tests "var" #f passes "var_test" (tests-for "var"))
(compiler-tests "dynamic" type-check-Lany passes "dynamic_test" (tests-for "dynamic"))


