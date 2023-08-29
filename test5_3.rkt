#lang racket
(require racket/set racket/stream)
(require graph)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(require "priority_queue.rkt")
(require "compiler.rkt")


(define p0
  (X86Program
   '((locals-types (sum . Integer) (i . Integer) (tmp3 . Integer) (tmp4 . Integer)))
   (list
    (cons 'start
          (Block '()
                 (list
                  (Instr 'movq (list (Imm 0) (Var 'sum)))
                  (Instr 'movq (list (Imm 5) (Var 'i)))
                  (Jmp 'block5))))

    (cons 'block5
          (Block '()
                 (list
                  (Instr 'movq (list (Var 'i) (Var 'tmp3)))
                  (Instr 'cmpq (list (Var 'tmp3) (Imm 0)))
                  (JmpIf 'l 'block7)
                  (Jmp 'block8))))

    (cons 'block7
          (Block '()
                 (list
                  (Instr 'addq (list (Var 'i) (Var 'sum)))
                  (Instr 'movq (list (Imm 1) (Var 'tmp4)))
                  (Instr 'negq (list (Var 'tmp4)))
                  (Instr 'addq (list (Var 'tmp4) (Var 'i)))
                  (Jmp 'block5))))
    (cons 'block8
          (Block '()
                 (list
                  (Instr 'movq (list (Imm 27) (Reg 'rax)))
                  (Instr 'addq (list (Var 'sum) (Reg 'rax)))
                  (Jmp 'conclusion)))))))

(define p1 (uncover-live p0))

(define p2 (build-interference p1))

(define p3 (allocate-registers p2))

(define p4 (patch-instructions p3))

(define p5 (prelude-and-conclusion p4))
