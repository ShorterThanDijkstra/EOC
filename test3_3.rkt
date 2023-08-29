#lang racket
(require graph)
(require "utilities.rkt")
(provide (all-defined-out))
(require "compiler.rkt")

;;; test exercise 3.3

(define p0
  (X86Program
 '((locals-types (x . Integer) (y . Integer) (z . Integer) (v . Integer) (w . Integer) (t . Integer)))
 (list
  (cons
   'start
   (Block
    '()
    (list
    (Instr 'movq (list (Imm 1) (Var 'v)))
    (Instr 'movq (list (Imm 42) (Var 'w)))
    (Instr 'movq (list (Var 'v) (Var 'x)))
    (Instr 'addq (list (Imm 7) (Var 'x)))
    (Instr 'movq (list (Var 'x) (Var 'y)))
    (Instr 'movq (list (Var 'x) (Var 'z)))
    (Instr 'addq (list (Var 'w) (Var 'z)))
    (Instr 'movq (list (Var 'y) (Var 't)))
    (Instr 'negq (list (Var 't)))
    (Instr 'movq (list (Var 'z) (Reg 'rax)))
    (Instr 'addq (list (Var 't) (Var 'rax)))
    (Jmp 'conclusion)))))))

(define p1 (uncover-live p0))

(define p2 (build-interference p1))

(define g (graph-of-program-start p2))

(display (graphviz g))