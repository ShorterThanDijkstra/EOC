#lang racket
(require graph)
(require "utilities.rkt")
(provide (all-defined-out))
(require "compiler.rkt")
(require "graph-printing.rkt")
(require "interp.rkt")


;;; for test
(define p0
  (X86Program
   '((spill-space . 0) (used-callee))
   (list
    (cons 'start
          (Block '()
                 (list
                  (Instr 'movq (list (Imm 1) (Reg 'rcx)))
                  (Jmp 'block_1))))

    (cons 'block_1
          (Block '()
                 (list (Instr 'movq (list (Imm 2) (Reg 'rcx)))
                       (Jmp 'block_2))))
    (cons 'block_2
          (Block '()
                 (list (Instr 'movq (list (Imm 3) (Reg 'rcx)))
                       (Jmp 'block_3))))
    (cons 'block_3
          (Block '()
                 (list (Instr 'movq (list (Imm 4) (Reg 'rcx)))
                       (Jmp 'conclusion)))))))


(define p1 (remove-jumps p0))

(define p2 (patch-instructions p1))

(define p3 (prelude-and-conclusion p2))

; (displayln (print-x86 p3))