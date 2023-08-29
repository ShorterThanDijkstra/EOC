#lang racket
(require racket/set
         racket/stream)
(require debug/repl) ; for debug
(require graph)
(require data/queue)
(require racket/promise)
(require "multigraph.rkt")
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(require "priority_queue.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))

;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-gen-sym)
  (let ([id 0])
    (lambda (sym)
      (if (eqv? sym 'main)
          sym
          (begin
            (set! id (+ id 1))
            (symbol-append sym
                           (symbol-append (string->symbol ".")
                                          (string->symbol
                                           (number->string id)))))))))

(define gen-sym (make-gen-sym))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Bool b) (Bool b)]
      [(Void) (Void)]
      [(Let x e body)
       (let ([new_x (gen-sym x)])
         (let ([new_env (dict-set env x new_x)])
           (Let new_x ((uniquify-exp env) e) ((uniquify-exp new_env) body))))]
      [(If cnd thn els)
       (If ((uniquify-exp env) cnd)
           ((uniquify-exp env) thn)
           ((uniquify-exp env) els))]
      [(Prim op es)
       (Prim op
             (for/list ([e es])
               ((uniquify-exp env) e)))]
      [(Apply f args)
       (Apply ((uniquify-exp env) f) (map (uniquify-exp env) args))]
      [(Lambda params rty body)
       (let ([new-env (for/fold ([env env]) ([param (map param-name params)])
                        (dict-set env param (gen-sym param)))])
         (Lambda (map (uniquify-param new-env) params)
                 rty
                 ((uniquify-exp new-env) body)))]
      [(SetBang var rhs) (SetBang (dict-ref env var) ((uniquify-exp env) rhs))]
      [(Begin exps body)
       (Begin (map (uniquify-exp env) exps) ((uniquify-exp env) body))]
      [(WhileLoop cnd body)
       (WhileLoop ((uniquify-exp env) cnd) ((uniquify-exp env) body))])))

(define ((uniquify-param env) param)
  (let ([name (param-name param)])
    (if (list? param)
        (cons (dict-ref env name) (cdr param))
        (dict-ref env name)))) ;;; dynamic typing

(define ((uniquify-def global-env) def)
  (match def
    [(Def name param* rty info body)
     (let ([new-env (for/fold ([env global-env])
                              ([param (map param-name param*)])
                      (dict-set env param (gen-sym param)))])
       (Def (dict-ref global-env name)
            (map (uniquify-param new-env) param*)
            rty
            info
            ((uniquify-exp new-env) body)))]))

;; uniquify : L -> L
(define (uniquify p)
  (match p
    [(ProgramDefs info defs)
     (let ([global-env (for/fold ([env '()]) ([f-name (map Def-name defs)])
                         (dict-set env f-name (gen-sym f-name)))])
       (ProgramDefs info (map (uniquify-def global-env) defs)))]))

; (define (show-exp e d)
;   (match e
;     [(Var x) (symbol->string x)]
;     [(Int n) (number->string n)]
;     [(Prim '- (list e)) (string-append "-" (show-exp e d))]
;     [(Prim '+ (list e1 e2))
;      (string-append "(" (show-exp e1 d) " + " (show-exp e2 d) ")")]
;     [(Prim '- (list e1 e2))
;      (string-append "(" (show-exp e1 d) " - " (show-exp e2 d) ")")]
;     [(Let x e body)
;      (let ([prefix (string-append "let " (symbol->string x) " = ")])
;        (string-append prefix
;                       (show-exp e (+ d (string-length prefix)))
;                       "\n"
;                       (make-string d #\ )
;                       "in "
;                       (show-exp body (+ d 3))))]))
; (define (show p)
;   (match p
;     [(Program info e) (display (show-exp e 0))]))

; let a = let b = 3 in let c = 4 in c + b
; in a

; x1: a
; e1: let b = 3 in let c = 4 in c + b
; body1: a

; x2: b
; e2: 3
; body2: let c = 4 in c + b
; (anf-exp (Let b 3 (Let a (Let c 4 (+ c b)) a)))

; x1: b
; e1: 3
; body1: let a = let c = 4 in c + b in a
; (Let b 3 (anf-exp (Let a (Let c 4 (+ c b)) a)))

; x1: a
; e1: let c = 4 in c + b
; body1: a

; x2: c
; e2: 4
; body2: c + b
; (Let b 3 (anf-exp (Let c 4 (Let a (+ c b) a))))

; (Let b 3 (Let c 4 (Let a (+ c b)) a))
(define (anf-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Prim op es) (Prim op es)]
    [(Let x1 e1 body1)
     (match e1
       [(Var _) (Let x1 e1 (anf-exp body1))]
       [(Int _) (Let x1 e1 (anf-exp body1))]
       [(Prim op es) (Let x1 e1 (anf-exp body1))] ; error
       [(Let x2 e2 body2) (anf-exp (Let x2 e2 (Let x1 body2 body1)))])]))
; (anf-exp (Let x2 e2 (anf-exp (Let x1 body2 (anf-exp body1)))))])]))

(define (rco-exp e)
  (define (rco-func-args func args)
    (let loop ([atm-args '()] [args args])
      (if (null? args)
          (if (atm? func)
              (Apply func (reverse atm-args))
              (let ([sym (gen-sym 'tmp)])
                (Let sym (rco-exp func) (Apply (Var sym) (reverse atm-args)))))
          (let ([arg (first args)])
            (if (atm? arg)
                (loop (cons arg atm-args) (rest args))
                (let ([sym (gen-sym 'tmp)] [new-arg (rco-exp arg)])
                  (Let sym
                       new-arg
                       (loop (cons (Var sym) atm-args) (rest args)))))))))
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(FunRef label n) (FunRef label n)]
    [(Apply func args)
     (if (atm? func)
         (rco-func-args func args)
         (let ([sym (gen-sym 'tmp)])
           (Let sym (rco-exp func) (rco-func-args (Var sym) args))))]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(If cnd thn els) (If (rco-exp cnd) (rco-exp thn) (rco-exp els))]
    [(Prim op es)
     (let loop ([es es] [atoms '()])
       (if (null? es)
           (Prim op (reverse atoms))
           (if (atm? (car es))
               (loop (cdr es) (cons (car es) atoms))
               (let ([sym (gen-sym 'tmp)] [new-e (rco-exp (car es))])
                 (Let sym new-e (loop (cdr es) (cons (Var sym) atoms)))))))]
    [(SetBang var rhs) (SetBang var (rco-exp rhs))]
    [(Begin exps body) (Begin (map rco-exp exps) (rco-exp body))]
    [(WhileLoop cnd body) (WhileLoop (rco-exp cnd) (rco-exp body))]
    ; [(GetBang var) (Var var)]))
    [(GetBang var) (GetBang var)]
    [(Allocate n ty) (Allocate n ty)]
    [(GlobalValue var) (GlobalValue var)]
    [(ValueOf e ftype)
     (if (atm? e)
         (ValueOf e ftype)
         (let ([tmp (gen-sym 'tmp)])
           (Let tmp (rco-exp e) (ValueOf (Var tmp) ftype))))]
    [(Exit) (Exit)]
    [(AllocateClosure len type arity) (AllocateClosure len type arity)]
    [(Collect n) (Collect n)]))

(define (rco-def def)
  (match def
    [(Def name param* rty info body)
     (Def name param* rty info (rco-exp body))]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map rco-def defs))]))

; [(Program info e) (Program info (anf-exp (rco-exp e)))]))
; (error "TODO: code goes here (remove-complex-opera*)"))

(define basic-blocks 'UNINTIATED)

(define (init-basic-blocks!)
  (set! basic-blocks '()))

; (define (create-block tail)
;   (match tail
;     [(Goto label) (Goto label)]
;     [else
;      (let ([label (gensym 'block)])
;        (set! basic-blocks (cons (cons label tail) basic-blocks))
;        (Goto label))]))

;;; 默认参数和输出都是promise,当需要构建ast时，force
(define (create-block tail)
  (delay (define t (forces tail))
         (match t
           [(Goto label) (Goto label)]
           [else
            (let ([label (gensym 'block)])
              (set! basic-blocks (cons (cons label t) basic-blocks))
              (Goto label))])))

(define (insert-block! label block)
  (let ([new-basic-blocks (cons (cons label block) basic-blocks)])
    (set! basic-blocks new-basic-blocks)))

(define (explicate-effect e cont)
  (delay
   (match e
     [(Var x) cont]
     [(Int n) cont]
     [(Bool b) cont]
     [(Void) cont]
     [(GetBang x) cont]
     [(Let var rhs body)
      (explicate-assign rhs var (explicate-effect body cont))]
     [(If cnd thn els)
      (explicate-pred cnd
                      (explicate-effect thn cont)
                      (explicate-effect els cont))]
     [(Prim 'read (list)) (Seq (Prim 'read (list)) (forces cont))]
     [(Prim 'vector-set! args) (Seq (Prim 'vector-set! args) (forces cont))]
     [(SetBang var rhs) (explicate-assign rhs var cont)]
     [(Begin exps body)
      ;;; todo for/foldr
      (let loop ([exps exps] [cont cont])
        (if (null? exps)
            cont
            (explicate-effect (first exps) (loop (rest exps) cont))))]
     [(WhileLoop cnd body)
      (let ([label (gensym 'loop)])
        (let ([res (Goto label)])
          (let ([loop-block
                 (explicate-pred cnd (explicate-effect body res) cont)])
            (insert-block! label (forces loop-block))
            res)))]
     [(Collect n) (Seq (Collect n) (forces cont))]
     [(GlobalValue var) cont]
     [(Prim op es) cont]
     [else (error "explicate-effect unhandled case" e)])))

(define (explicate-pred cnd thn els)
  (delay
   (match (forces cnd)
     [(Var x)
      (let ([thn-goto (forces (create-block thn))]
            [els-goto (forces (create-block els))])
        ;  (IfStmt cnd thn-goto els-goto))] ;;; pred must be a Prim of cmp
        (IfStmt (Prim 'eq? (list (Var x) (Bool #t))) thn-goto els-goto))]
     [(Apply func args)
      (let ([sym (gen-sym 'tmp)])
        (explicate-assign (Apply func args)
                          sym
                          (explicate-pred (Var sym) thn els)))]
     [(GetBang x)
      (let ([thn-goto (forces (create-block thn))]
            [els-goto (forces (create-block els))])
        (IfStmt (Prim 'eq? (list (Var x) (Bool #t))) thn-goto els-goto))]
     [(Let x rhs body) (explicate-assign rhs x (explicate-pred body thn els))]
     [(Prim 'not (list e)) (explicate-pred e els thn)]
     [(Prim op es)
      #:when
      (or (eq? op 'eq?) (eq? op '<) (eq? op '<=) (eq? op '>) (eq? op '>=))
      (IfStmt (Prim op es)
              (forces (create-block thn))
              (forces (create-block els)))]
     [(Bool b) (if b thn els)]
     [(If cnd^ thn^ els^)
      (let ([thn-goto (create-block thn)] [els-goto (create-block els)])
        (let ([thn^-goto (create-block (explicate-pred thn^ thn-goto els-goto))]
              [els^-goto (create-block
                          (explicate-pred els^ thn-goto els-goto))])
          (explicate-pred cnd^ thn^-goto els^-goto)))]
     [(Begin exps body)
      ;;; todo for/foldr
      (let loop ([exps exps] [cont (explicate-pred body cnd thn els)])
        (if (null? exps)
            cont
            (explicate-effect (first exps) (loop (rest exps) cont))))]
     [(ValueOf e ftype)
      (let ([tmp (gen-sym 'tmp)])
        (explicate-assign (ValueOf e ftype)
                          tmp
                          (explicate-pred (Var tmp) thn els)))]
     [else (error "explicate-pred unhandled case" cnd)])))

(define (explicate-tail e-lazy)
  (delay
   (define e (forces e-lazy))
   (match e
     [(Var x) (Return e)]
     [(Int n) (Return e)]
     [(Bool b) (Return e)]
     [(Void) (Return e)]
     [(Exit) (Exit)]
     [(GetBang x) (Return (Var x))]
     [(Prim op es) (Return e)]
     [(FunRef label n) (Return e)]
     [(Apply func args) (TailCall func args)]
     [(Let x rhs body) (explicate-assign rhs x (explicate-tail body))]
     [(If cnd thn els)
      (explicate-pred cnd (explicate-tail thn) (explicate-tail els))]
     [(ValueOf e ftype)
      (let ([tmp (gen-sym 'tmp)])
        (explicate-assign (ValueOf e ftype) tmp (explicate-tail (Var tmp))))]
     [(WhileLoop cnd body)
      (let ([label (gensym 'loop)])
        (let ([res (Goto label)])
          ; (debug-repl)
          (let ([loop-block (explicate-pred cnd
                                            (explicate-effect body res)
                                            (Return (Void)))])
            (insert-block! label (forces loop-block))
            res)))]
     [(Begin exps body)
      ;;; todo for/foldr
      (let loop ([exps exps] [cont (explicate-tail body)])
        (if (null? exps)
            cont
            (explicate-effect (first exps) (loop (rest exps) cont))))]
     [(SetBang var rhs) (explicate-assign rhs var (Return (Void)))]
     [(Allocate n ty) (Return (Allocate n ty))]
     [(AllocateClosure len ty arity) (Return (AllocateClosure len ty arity))]
     [(GlobalValue var) (Return (GlobalValue var))]
     [else (error "explicate-tail unhandled case" e)])))

(define (explicate-assign e-lazy x cont)
  (delay
   (define e (forces e-lazy))
   (match e
     [(Var y) (Seq (Assign (Var x) e) (forces cont))]
     [(Apply func args) (Seq (Assign (Var x) (Call func args)) (forces cont))]
     [(FunRef label n) (Seq (Assign (Var x) (FunRef label n)) (forces cont))]
     [(GetBang y) (Seq (Assign (Var x) (Var y)) (forces cont))]
     [(Int n) (Seq (Assign (Var x) e) (forces cont))]
     [(Bool n) (Seq (Assign (Var x) e) (forces cont))]
     [(Void) (Seq (Assign (Var x) e) (forces cont))]
     [(Prim op es) (Seq (Assign (Var x) e) (forces cont))]
     [(Let y rhs body) (explicate-assign rhs y (explicate-assign body x cont))]
     [(ValueOf e ftype) (Seq (Assign (Var x) e) (forces cont))]
     [(If cnd thn els)
      (let ([new-cont (create-block cont)])
        (let ([new-thn (explicate-assign thn x new-cont)]
              [new-els (explicate-assign els x new-cont)])
          (explicate-pred cnd new-thn new-els)))]
     [(WhileLoop cnd body)
      (let ([label (gensym 'loop)])
        (let ([res (Goto label)])
          (let ([loop-block (explicate-pred cnd
                                            (explicate-effect body res)
                                            (explicate-assign (Void) x cont))])
            (insert-block! label (forces loop-block))
            res)))]
     [(Exit) (Exit)]

     [(Begin exps body)
      (let loop ([exps exps] [cont^ (explicate-assign body x cont)])
        (if (null? exps)
            cont^
            (explicate-effect (first exps) (loop (rest exps) cont^))))]
     [(SetBang var rhs)
      (explicate-assign rhs var (explicate-assign (Void) x cont))]
     [(Allocate n ty) (Seq (Assign (Var x) (Allocate n ty)) (forces cont))]
     [(AllocateClosure len ty arity)
      (Seq (Assign (Var x) (AllocateClosure len ty arity)) (forces cont))]
     [(Collect n) (Seq (Collect n) (forces cont))]
     [(GlobalValue var) (Seq (Assign (Var x) (GlobalValue var)) (forces cont))]
     [else (error "explicate-assign unhandled case" e)])))

(define (forces p)
  (if (promise? p) (forces (force p)) p))

; ;; explicate-control : L -> C
; (define (explicate-control p)
;   (init-basic-blocks!)
;   (match p
;     [(Program info body)
;      (let ([block (forces (explicate-tail body))]) (CProgram info (cons (cons 'start block) basic-blocks)))]))

(define (explicate-control-def def)
  (init-basic-blocks!)
  (match def
    [(Def name param* rty info body)
     (let ([tail (forces (explicate-tail body))])
       (Def name
            param*
            rty
            info
            (cons (cons (symbol-append name 'start) tail) basic-blocks)))]))

(define (explicate-control p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map explicate-control-def defs))]))

(define (insts-atm c-ele)
  (match c-ele
    [(Int n) (Imm n)]
    [(Var x) (Var x)]
    [(Bool n) (if n (Imm 1) (Imm 0))]
    [(Void) (Imm 0)]
    [else (error 'insts-atm)]))

; (define (insts-exp c-ele)
;   (match c-ele
;     [(Prim 'read '()) (list (Callq 'read_int) 0)]
;     [(Prim '- (list atm)) (list (Instr 'negq (insts-atm atm)))]
;     [(Prim '+ (list atm1 atm2)) (list (Instr 'movq (insts-atm atm1) (Reg 'rax))
;                                       (Instr 'addq (insts-atm atm2) (Reg 'rax)))]
;     [(Prim '- (list atm1 atm2)) (list (Instr 'movq (insts-atm atm1) (Reg 'rax))
;                                       (Instr 'subq (insts-atm atm2) (Reg 'rax)))]
;     [else (error 'insts-exp)]))
(define (eq-var? var sym)
  (match var
    [(Var x) (eqv? x sym)]
    [else #f]))

;;; 变量对应的label
(define sym->label 'UNINITIALIZED)

(define (init!-sym->label)
  (set! sym->label '()))

(define (update!-sym->label sym label)
  (let ([new-sym->label (dict-set sym->label sym label)])
    (set! sym->label new-sym->label)))

; store result in %rax
(define (insts-exp e)
  (match e
    [(Var x) (list (Instr 'movq (list (Var x) (Reg 'rax))))]
    [(Int n) (list (Instr 'movq (list (Imm n) (Reg 'rax))))]
    [(Bool b) (list (Instr 'movq (list (Imm 1) (Reg 'rax))))]
    [(Void) (list (Instr 'movq (list (Imm 0) (Reg 'rax))))]
    [(FunRef label n) (list (Instr 'leaq (list (Global label) (Reg 'rax))))]
    [(Call func args)
     (define func-local
       (match (insts-atm func)
         [(Var x) (Var x)]
         [else (error 'insts-exp "expected Var")]))
     (let ([num-params (length args)])
       (let ([movs (for/list ([arg args] [reg (take argument-pass num-params)])
                     (Instr 'movq (list (insts-atm arg) (Reg reg))))]
             [call-instr (IndirectCallq func-local num-params)])
         (append movs (list call-instr))))]
    [(Allocate n ty)
     (let ([len (- (length ty) 1)])
       (let ([mask-tag
              (for/fold ([tag 0]) ([ele-ty (rest ty)])
                (let ([new-tag (arithmetic-shift tag 1)])
                  (if (vector-type? ele-ty) (bitwise-ior new-tag 1) new-tag)))])
         (let ([len-tag (bitwise-ior (arithmetic-shift mask-tag 6) len)])
           (let ([tag (bitwise-ior (arithmetic-shift len-tag 1)
                                   1)]) ; not copied
             (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
                   (Instr 'addq (list (Imm (* 8 (+ len 1))) (Global 'free_ptr)))
                   (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
                   (Instr 'movq (list (Reg 'r11) (Reg 'rax))))))))]
    [(AllocateClosure len ty arity)
     (let ([mask-tag (let ([mask (for/fold ([tag 0]) ([ele-ty (rest ty)])
                                   (let ([new-tag (arithmetic-shift tag 1)])
                                     (if (vector-type? ele-ty)
                                         (bitwise-ior new-tag 1)
                                         new-tag)))])
                       (bitwise-ior (arithmetic-shift arity 5) mask))])
       (let ([len-tag (bitwise-ior (arithmetic-shift mask-tag 6) len)])
         (let ([tag (bitwise-ior (arithmetic-shift len-tag 1) 1)])
           (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
                 (Instr 'addq (list (Imm (* 8 (+ len 1))) (Global 'free_ptr)))
                 (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
                 (Instr 'movq (list (Reg 'r11) (Reg 'rax)))))))]
    [(Prim 'make-any (list e (Int tag)))
     (let ([lhs (Reg 'rax)])
       (if (vector-or-proc-tag? tag)
           (list (Instr 'movq (list (insts-atm e) lhs))
                 (Instr 'orq (list (Imm tag) lhs)))
           (list (Instr 'movq (list (insts-atm e) lhs))
                 (Instr 'salq (list (Imm 3) lhs))
                 (Instr 'orq (list (Imm tag) lhs)))))]
    [(Prim 'tag-of-any (list e))
     (let ([lhs (Reg 'rax)])
       (list (Instr 'movq (list (insts-atm e) lhs))
             (Instr 'andq (list (Imm 7) lhs))))]
    [(Prim 'any-vector-length (list e))
     (let ([lhs (Reg 'rax)])
       (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
             (Instr 'andq (list (insts-atm e) (Reg 'r11)))
             (Instr 'movq (list (Deref 'r11 0) (Reg 'r11)))
             (Instr 'andq
                    (list (Imm 176) (Reg 'r11))) ;;; 176 = 0x126 = 0b1111110
             (Instr 'sarq (list (Imm 1) (Reg 'r11)))
             (Instr 'movq (list (Reg 'r11) lhs))))]
    [(ValueOf e ftype)
     (let ([lhs (Reg 'rax)])
       (if (vector-or-proc-tag? (tagof ftype))
           (list (Instr 'movq (list (insts-atm e) lhs))
                 (Instr 'sarq (list (Imm 3) lhs)))
           (list (Instr 'movq (list (Imm -8) lhs))
                 (Instr 'andq (list (insts-atm e) lhs)))))]
    [(Prim 'any-vector-ref (list e1 e2))
     (let ([lhs (Reg 'rax)])
       (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
             (Instr 'andq (list (insts-atm e1) (Reg 'r11)))
             (Instr 'movq (list (insts-atm e2) (Reg 'rax)))
             (Instr 'addq (list (Imm 1) (Reg 'rax))) ;;; first 8 bytes are marks
             (Instr 'imulq (list (Imm 8) (Reg 'rax)))
             (Instr 'addq (list (Reg 'rax) (Reg 'r11)))
             (Instr 'movq (list (Deref 'r11 0) lhs))))]
    [(Prim 'any-vector-set! (list e1 e2 e3))
     (let ([lhs (Reg 'rax)])
       (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
             (Instr 'andq (list (insts-atm e1) (Reg 'r11)))
             (Instr 'movq (list (insts-atm e2) (Reg 'rax)))
             (Instr 'addq (list (Imm 1) (Reg 'rax))) ;;; first 8 bytes are marks
             (Instr 'imulq (list (Imm 8) (Reg 'rax)))
             (Instr 'addq (list (Reg 'rax) (Reg 'r11)))
             (Instr 'movq (list (insts-atm e3) (Deref 'r11 0)))
             (Instr 'movq (list (Imm 0) lhs))))] ; void
    [(Prim 'procedure-arity (list atm))
     (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
           (Instr 'movq (list (Deref 'r11 0) (Reg 'rax)))
           (Instr 'sarq (list (Imm 1) (Reg 'rax)))
           (Instr 'andq (list (Imm 31) (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) (Reg 'rax))))]
    [(Prim 'vector-ref (list atm (Int n)))
     (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
           (Instr 'movq (list (Deref 'r11 (* (+ 1 n) 8)) (Reg 'rax))))]
    [(Prim 'vector-set! (list atm1 (Int n) atm2))
     (list (Instr 'movq (list (insts-atm atm1) (Reg 'r11)))
           (Instr 'movq (list (insts-atm atm2) (Deref 'r11 (* 8 (+ n 1)))))
           (Instr 'movq (list (Imm 0) (Reg 'rax))))]
    [(Prim 'vector-length (list atm))
     (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
           (Instr 'movq (list (Deref 'r11 7) (Reg 'rax)))
           (Instr 'sarq (list (Imm 1) (Reg 'rax)))
           (Instr 'andq (list (Imm 63) (Reg 'rax))))]
    [(GlobalValue var) (list (Instr 'movq (list (Global var) (Reg 'rax))))]
    [(Prim 'read '()) (list (Callq 'read_int 0))]
    [(Prim '- (list atm))
     (list (Instr 'movq (list (insts-atm atm) (Reg 'rax)))
           (Instr 'negq (list (Reg 'rax))))]
    [(Prim '+ (list atm1 atm2))
     (list (Instr 'movq (list (insts-atm atm1) (Reg 'rax)))
           (Instr 'addq (list (insts-atm atm2) (Reg 'rax))))]
    [(Prim '- (list atm1 atm2))
     (list (Instr 'movq (list (insts-atm atm1) (Reg 'rax)))
           (Instr 'subq (list (insts-atm atm2) (Reg 'rax))))]
    [(Prim 'not (list atm1))
     (let ([arg1 (insts-atm atm1)])
       (list (Instr 'movq (list arg1 (Reg 'rax)))
             (Instr 'xorq (list (Imm 1) (Reg 'rax)))))]
    [(Prim 'eq? (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'e (Reg 'rax))]
    [(Prim '< (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'l (Reg 'rax))]
    [(Prim '<= (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'le (Reg 'rax))]
    [(Prim '> (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'g (Reg 'rax))]
    [(Prim '>= (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'ge (Reg 'rax))]
    [else (error "insts-exp: ~s" e)]))

(define (instr-prim-cmp atm1 atm2 cc to)
  (let ([arg1 (insts-atm atm1)] [arg2 (insts-atm atm2)])
    (list (Instr 'cmpq (list arg2 arg1))
          (Instr 'set (list cc (ByteReg 'al)))
          (Instr 'movzbq (list (ByteReg 'al) to)))))

(define (insts-stmt c-ele)
  (match c-ele
    [(Prim 'read (list)) (list (Callq 'read_int 0))]
    [(Prim 'vector-set! (list atm1 (Int n) atm2))
     (list (Instr 'movq (list (insts-atm atm1) (Reg 'r11)))
           (Instr 'movq (list (insts-atm atm2) (Deref 'r11 (* 8 (+ n 1))))))]
    [(Collect n)
     (list (Instr 'movq (list (Reg 'r15) (Reg 'rdi)))
           (Instr 'movq (list (Imm n) (Reg 'rsi)))
           (Callq 'collect 2))]
    [(Assign (Var x) e)
     (match e
       [(Var x1) (list (Instr 'movq (list (Var x1) (Var x))))]
       [(Int n) (list (Instr 'movq (list (Imm n) (Var x))))]
       [(Bool b) (list (Instr 'movq (list (Imm 1) (Var x))))]
       [(Void) (list (Instr 'movq (list (Imm 0) (Var x))))]
       [(FunRef label n)
        (begin
          (update!-sym->label x label)
          (list (Instr 'leaq (list (Global label) (Var x)))))]
       [(Call func args)
        (define func-local
          (match (insts-atm func)
            [(Var x) (Var x)]
            [else (error 'insts-stmt "expected Var")]))
        (let ([num-params (length args)])
          (let ([movs (for/list ([arg args]
                                 [reg (take argument-pass num-params)])
                        (Instr 'movq (list (insts-atm arg) (Reg reg))))]
                [call-instr (IndirectCallq func-local num-params)]
                [final (Instr 'movq (list (Reg 'rax) (Var x)))])
            (append movs (list call-instr final))))]
       [(AllocateClosure len ty arity)
        (let ([mask-tag (let ([mask (for/fold ([tag 0]) ([ele-ty (rest ty)])
                                      (let ([new-tag (arithmetic-shift tag 1)])
                                        (if (vector-type? ele-ty)
                                            (bitwise-ior new-tag 1)
                                            new-tag)))])
                          (bitwise-ior (arithmetic-shift arity 5) mask))])
          (let ([len-tag (bitwise-ior (arithmetic-shift mask-tag 6) len)])
            (let ([tag (bitwise-ior (arithmetic-shift len-tag 1) 1)])
              (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
                    (Instr 'addq
                           (list (Imm (* 8 (+ len 1))) (Global 'free_ptr)))
                    (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
                    (Instr 'movq (list (Reg 'r11) (Var x)))))))]
       [(Allocate n ty)
        (let ([len (- (length ty) 1)])
          (let ([mask-tag (for/fold ([tag 0]) ([ele-ty (rest ty)])
                            (let ([new-tag (arithmetic-shift tag 1)])
                              (if (vector-type? ele-ty)
                                  (bitwise-ior new-tag 1)
                                  new-tag)))])
            (let ([len-tag (bitwise-ior (arithmetic-shift mask-tag 6) len)])
              (let ([tag (bitwise-ior (arithmetic-shift len-tag 1)
                                      1)]) ; not copied
                (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
                      (Instr 'addq
                             (list (Imm (* 8 (+ len 1))) (Global 'free_ptr)))
                      (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
                      (Instr 'movq (list (Reg 'r11) (Var x))))))))]
       [(Prim 'make-any (list e (Int tag)))
        (let ([lhs (Var x)])
          (if (vector-or-proc-tag? tag)
              (list (Instr 'movq (list (insts-atm e) lhs))
                    (Instr 'orq (list (Imm tag) lhs)))
              (list (Instr 'movq (list (insts-atm e) lhs))
                    (Instr 'salq (list (Imm 3) lhs))
                    (Instr 'orq (list (Imm tag) lhs)))))]
       [(Prim 'tag-of-any (list e))
        (let ([lhs (Var x)])
          (list (Instr 'movq (list (insts-atm e) lhs))
                (Instr 'andq (list (Imm 7) lhs))))]
       [(ValueOf e ftype)
        (let ([lhs (Var x)])
          (if (vector-or-proc-tag? (tagof ftype))
              (list (Instr 'movq (list (insts-atm e) lhs))
                    (Instr 'sarq (list (Imm 3) lhs)))
              (list (Instr 'movq (list (Imm -8) lhs))
                    (Instr 'andq (list (insts-atm e) lhs)))))]
       [(Prim 'any-vector-length (list e))
        (let ([lhs (Var x)])
          (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
                (Instr 'andq (list (insts-atm e) (Reg 'r11)))
                (Instr 'movq (list (Deref 'r11 0) (Reg 'r11)))
                (Instr 'andq
                       (list (Imm 176) (Reg 'r11))) ;;; 176 = 0x126 = 0b1111110
                (Instr 'sarq (list (Imm 1) (Reg 'r11)))
                (Instr 'movq (list (Reg 'r11) lhs))))]
       [(Prim 'any-vector-ref (list e1 e2))
        (let ([lhs (Var x)])
          (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
                (Instr 'andq (list (insts-atm e1) (Reg 'r11)))
                (Instr 'movq (list (insts-atm e2) (Reg 'rax)))
                (Instr 'addq
                       (list (Imm 1) (Reg 'rax))) ;;; first 8 bytes are marks
                (Instr 'imulq (list (Imm 8) (Reg 'rax)))
                (Instr 'addq (list (Reg 'rax) (Reg 'r11)))
                (Instr 'movq (list (Deref 'r11 0) lhs))))]
       [(Prim 'any-vector-set! (list e1 e2 e3))
        (let ([lhs (Var x)])
          (list (Instr 'movq (list (Imm -8) (Reg 'r11)))
                (Instr 'andq (list (insts-atm e1) (Reg 'r11)))
                (Instr 'movq (list (insts-atm e2) (Reg 'rax)))
                (Instr 'addq
                       (list (Imm 1) (Reg 'rax))) ;;; first 8 bytes are marks
                (Instr 'imulq (list (Imm 8) (Reg 'rax)))
                (Instr 'addq (list (Reg 'rax) (Reg 'r11)))
                (Instr 'movq (list (insts-atm e3) (Deref 'r11 0)))
                (Instr 'movq (list (Imm 0) lhs))))] ; void
       [(Prim 'procedure-arity (list atm))
        (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
              (Instr 'movq (list (Deref 'r11 0) (Reg 'rax)))
              (Instr 'sarq (list (Imm 1) (Reg 'rax)))
              (Instr 'andq (list (Imm 31) (Reg 'rax)))
              (Instr 'movq (list (Reg 'rax) (Var x))))]
       [(Prim 'vector-ref (list atm (Int n)))
        (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
              (Instr 'movq (list (Deref 'r11 (* (+ 1 n) 8)) (Var x))))]
       [(Prim 'vector-set! (list atm1 (Int n) atm2))
        (list (Instr 'movq (list (insts-atm atm1) (Reg 'r11)))
              (Instr 'movq (list (insts-atm atm2) (Deref 'r11 (* 8 (+ n 1)))))
              (Instr 'movq (list (Imm 0) (Var x))))]
       [(Prim 'vector-length (list atm))
        (list (Instr 'movq (list (insts-atm atm) (Reg 'r11)))
              (Instr 'movq (list (Deref 'r11 7) (Reg 'rax)))
              (Instr 'sarq (list (Imm 1) (Reg 'rax)))
              (Instr 'andq (list (Imm 63) (Reg 'rax)))
              (Instr 'movq (list (Reg 'rax) (Var x))))]
       [(GlobalValue var)
        (list (Instr 'movq (list (Global var) (Var x))))] ; todo : right?
       [(Prim 'read '())
        (list (Callq 'read_int 0) (Instr 'movq (list (Reg 'rax) (Var x))))]
       [(Prim '- (list atm))
        (list (Instr 'movq (list (insts-atm atm) (Var x)))
              (Instr 'negq (list (Var x))))]
       [(Prim '+ (list atm1 atm2))
        (if (eq-var? atm1 x)
            (list (Instr 'addq (list (insts-atm atm2) (Var x))))
            (if (eq-var? atm2 x)
                (list (Instr 'addq (list (insts-atm atm1) (Var x))))
                (list (Instr 'movq (list (insts-atm atm1) (Var x)))
                      (Instr 'addq (list (insts-atm atm2) (Var x))))))]
       [(Prim '- (list atm1 atm2))
        (if (eq-var? atm1 x)
            (list (Inst 'subq (list (insts-atm atm2) (Var x))))
            (list (Instr 'movq (list (insts-atm atm1) (Var x)))
                  (Instr 'subq (list (insts-atm atm2) (Var x)))))]
       [(Prim 'not (list atm1))
        (match atm1
          [(Var sym)
           #:when (eqv? sym x)
           (list (Instr 'xnorq (list (Imm 1) (Var sym))))]
          [else
           (let ([arg1 (insts-atm atm1)])
             (list (Instr 'movq (list arg1 (Var x)))
                   (Instr 'xorq (list (Imm 1) (Var x)))))])]
       [(Prim 'eq? (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'e (Var x))]
       [(Prim '< (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'l (Var x))]
       [(Prim '<= (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'le (Var x))]
       [(Prim '> (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'g (Var x))]
       [(Prim '>= (list atm1 atm2)) (instr-prim-cmp atm1 atm2 'ge (Var x))])]
    [else (error 'insts-stmt)]))

(define (instr-tail-prim-cmp atm1 atm2 thn-label els-label cmp)
  (define (build arg1 arg2 cc)
    (list (Instr 'cmpq (list arg2 arg1)) (JmpIf cc thn-label) (Jmp els-label)))
  (define (cmp->cc)
    (match cmp
      ['eq? 'e]
      ['< 'l]
      ['<= 'le]
      ['> 'g]
      ['>= 'ge]))
  (let ([arg1 (insts-atm atm1)] [arg2 (insts-atm atm2)])
    (build arg1 arg2 (cmp->cc))))

(define (insts-tail c-ele name)
  (match c-ele
    [(Return e)
     (append (insts-exp e) (list (Jmp (symbol-append name 'conclusion))))]
    [(Goto label) (list (Jmp label))]
    [(Exit) (list (Instr 'movq (list (Imm -1) (Reg 'rdi))) (Callq 'exit 1))]
    [(TailCall func args)
     (define func-local
       (match (insts-atm func)
         [(Var x) (Var x)]
         [else (error 'insts-tail "expected Var")]))
     (let ([num-params (length args)])
       (let ([movs (for/list ([arg args] [reg (take argument-pass num-params)])
                     (Instr 'movq (list (insts-atm arg) (Reg reg))))]
             [jump-instr (TailJmp func-local num-params)])
         (append movs (list jump-instr))))]
    [(IfStmt (Prim cmp (list atm1 atm2)) (Goto thn-label) (Goto els-label))
     (instr-tail-prim-cmp atm1 atm2 thn-label els-label cmp)]
    [(Seq stmt tail) (append (insts-stmt stmt) (insts-tail tail name))]
    [else (error 'insts-tail)]))

; (define (insts c-ele)
;   (match c-ele
;     [(Int n) (insts-atm c-ele)]
;     [(Var x) (insts-atm c-ele)]
;     [(Bool b) (insts-atm c-ele)]
;     [(Void) (insts-atm c-ele)]
;     [(Prim op es) (insts-exp c-ele)]
;     [(Assign (Var x) e) (insts-stmt c-ele)]
;     [(Return e) (insts-tail c-ele)]
;     [(Goto label) (insts-tail c-ele)]
;     [(IfStmt pred thn els) (insts-tail c-ele)]
;     [(Seq stmt tail) (insts-tail c-ele)]))

;; select-instructions : C0 -> pseudo-x86
; (define (select-instructions p)
;   (match p
;     [(CProgram info label&blocks)
;      (X86Program info
;                  (for/list ([label&block label&blocks])
;                    (cons (car label&block)
;                          (Block '() (insts-tail (cdr label&block))))))]))

(define (move-args-regs-to-locals tail param-names)
  (let ([args-regs (take argument-pass (length param-names))])
    (append (map (lambda (arg-reg param-name)
                   (Instr 'movq (list (Reg arg-reg) (Var param-name))))
                 args-regs
                 param-names)
            tail)))

(define (select-instruction-block label name tail param-names)
  (if (eqv? label (symbol-append name 'start))
      (cons label (Block '() (move-args-regs-to-locals tail param-names)))
      (cons label (Block '() tail))))

(define (select-instruction-def def)
  (init!-sym->label)
  (match def
    [(Def name param* rty info labels&tails)
     (let ([num-params (length param*)] [param-names (map car param*)])
       (let ([new-labels&tails
              (for/list ([label&tail labels&tails])
                (let ([label (car label&tail)] [tail (cdr label&tail)])
                  (select-instruction-block label
                                            name
                                            (insts-tail tail name)
                                            param-names)))])
         (Def name
              '()
              'Integer
              (dict-set (dict-set info 'num-params num-params)
                        'sym->label
                        sym->label)
              new-labels&tails)))]))

(define (select-instructions p)
  (match p
    [(ProgramDefs info defs)
     (X86Program info (map select-instruction-def defs))]))

; (error "TODO: code goes here (select-instructions)"))

(define (type-space type)
  (match type
    ['Integer 8]
    ['Boolean 8]
    ['Void 8]
    [(cons 'Vector ts)
     (+ 8
        (for/fold ([size 0]) ([t ts])
          (+ size (type-space t))))]
    [(cons t1 ts) 8] ;;; function type
    [else (error "type-space ~s" type)]))

; homes: listof (sym . int)
(define (assign-arg arg locals-types homes)
  (match arg
    [(Imm n) (list (Imm n) homes)]
    [(Reg reg) (list (Reg reg) homes)]
    [(Deref reg n) (list (Deref reg n) homes)]
    [(ByteReg reg) (list (ByteReg reg) homes)]
    [(Var x)
     (let ([exist (dict-ref homes x #f)])
       (if exist
           (list (Deref 'rbp exist) homes)
           (let ([type (dict-ref locals-types x)]
                 [top (if (null? homes) 0 (cdar homes))])
             (let ([new-top (- top (type-space type))])
               (let ([new-homes (cons (cons x new-top) homes)])
                 (list (Deref 'rbp new-top) new-homes))))))]
    [else (error 'assign-arg)]))

(define (assign-instr instr locals-types homes)
  (match instr
    [(Instr name args)
     (let loop ([args args] [new-args '()] [homes homes])
       (if (null? args)
           (list (Instr name (reverse new-args)) homes)
           (let ([res (assign-arg (car args) locals-types homes)])
             (let ([new-arg (car res)] [new-homes (cadr res)])
               (loop (cdr args) (cons new-arg new-args) new-homes)))))]
    [(Callq label n) (list (Callq label n) homes)]
    [(Retq) (list (Retq) homes)]
    [(Jmp label) (list (Jmp label) homes)]
    [(JmpIf cc label) (list (JmpIf cc label) homes)]
    [else (error 'assign-instr)]))

(define (align16 n)
  (let ([m (modulo n 16)]) (if (zero? m) n (+ n (- 16 m)))))

(define (assign-block block locals-types)
  (match block
    [(Block info instrs)
     ;  (displayln instrs)
     (let loop ([instrs instrs] [new-instrs '()] [homes '()])
       (if (null? instrs)
           (let ([top (if (null? homes) 0 (cdar homes))])
             (list (align16 (- top)) (reverse new-instrs)))
           (let ([res (assign-instr (car instrs) locals-types homes)])
             (loop (cdr instrs) (cons (car res) new-instrs) (cadr res)))))]
    [else (error 'assign-block "~s" block)]))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program info label&blocks)
     (let ([locals-types (dict-ref info 'locals-types)])
       (let loop ([label&blocks label&blocks]
                  [labels '()]
                  [block-infos '()]
                  [block-instrs '()]
                  [spaces '()])
         (if (null? label&blocks)
             (X86Program (append (list (cons 'stack-space spaces)) info)
                         (for/list ([label labels]
                                    [block-info block-infos]
                                    [instrs block-instrs])
                           (cons label (Block block-info instrs))))
             (let ([curr-label (caar label&blocks)]
                   [curr-block (cdar label&blocks)])
               ;  (displayln curr-label)
               ;  (displayln curr-block)
               (let ([res (assign-block curr-block locals-types)]
                     [block-info (Block-info curr-block)])
                 (let ([space (car res)] [new-block-instrs (cadr res)])
                   (loop (cdr label&blocks)
                         (cons curr-label labels)
                         (cons block-info block-infos)
                         (cons new-block-instrs block-instrs)
                         (cons (cons curr-label space) spaces))))))))]))

(define (deref-args-instr? instr)
  (match instr
    [(Instr 'movq (list (Deref reg1 n1) (Deref reg2 n2))) #t]
    [(Instr 'movzbq (list (Deref reg1 n1) (Deref reg2 n2))) #t]
    [else #f]))

(define (arg-equal? arg1 arg2)
  (match* (arg1 arg2)
    [((Var name1) (Var name2)) (eqv? name1 name2)]
    [((Reg name1) (Reg name2)) (eqv? name1 name2)]
    [((Imm n1) (Imm n2)) (eqv? n1 n2)]
    [((Deref name1 offset1) (Deref name2 offset2))
     (and (eqv? name1 name2) (eqv? offset1 offset2))]
    [((Global label1) (Global label2)) (eqv? label1 label2)]
    [(_ _) #f]))

(define (trival-mov? instr)
  (match instr
    [(Instr 'movq (list arg1 arg2)) (arg-equal? arg1 arg2)]
    [else #f]))

(define (patch-instr instr)
  (match instr
    [(Instr 'addq (list deref1 deref2))
     (list (Instr 'movq (list deref1 (Reg 'rax)))
           (Instr 'addq (list (Reg 'rax) deref2)))]
    [(Instr 'subq (list deref1 deref2))
     (list (Instr 'movq (list deref1 (Reg 'rax)))
           (Instr 'subq (list (Reg 'rax) deref2)))]
    [(Instr 'movq (list deref1 deref2))
     (list (Instr 'movq (list deref1 (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) deref2)))]
    [(Instr 'movzbq (list deref1 deref2))
     (list (Instr 'movzbq (list deref1 (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) deref2)))]
    [(Instr 'cmpq (list deref1 deref2))
     (list (Instr 'movq (list deref1 (Reg 'rax)))
           (Instr 'cmpq (list (Reg 'rax) deref2)))]
    [else (error 'patch-instr)]))

(define (problematic-cmpq? instr)
  (match instr
    [(Instr 'cmpq (list arg1 (Imm n2))) #t]
    [else #f]))

(define (problematic-leaq? instr)
  (match instr
    [(Instr 'leaq (list arg1 arg2)) (not (Reg? arg2))]
    [else #f]))

(define (problematic-tailjmp? instr)
  (match instr
    [(TailJmp (Reg 'rax) n) #f]
    [(TailJmp arg n) #t]
    [else #f]))

(define (patch-problematic-tailjmp instr)
  (match instr
    [(TailJmp (Reg name) n)
     (list (Instr 'movq (list (Reg name) (Reg 'rax))) (TailJmp (Reg 'rax) n))]))

(define (patch-problematic-cmpq instr)
  (match instr
    [(Instr 'cmpq (list arg1 (Imm n2)))
     (list (Instr 'movq (list (Imm n2) (Reg 'rax)))
           (Instr 'cmpq (list arg1 (Reg 'rax))))]
    [else (error 'patch-problematic-cmpq)]))

(define (patch-problematic-leaq instr)
  (match instr
    [(Instr 'leaq (list arg1 (Deref sym offset)))
     (list (Instr 'movq (list (Deref sym offset) (Reg 'rax)))
           (Instr 'leaq (list arg1 (Reg 'rax))))]
    [else (error "patch-problematic-leaq: ~s" instr)]))

; (define (patch-instr instr)
;   (match instr
;     [(Instr name args)
;      (if (deref-pair? args)
;          (patch-instr-arg instr)
;          (Instr name args))]
;     [(Callq label n) (Callq label n) ]
;     [(Retq) (Retq) ]
;     [(Jmp label) (Jmp label) ]
;     [else (error 'assign-instr)]))

(define (patch block)
  (match block
    [(Block info instrs)
     (Block
      info
      (let loop ([instrs instrs] [new-instrs '()])
        (if (null? instrs)
            (reverse new-instrs)
            (cond
              [(trival-mov? (first instrs)) (loop (rest instrs) new-instrs)]
              [(deref-args-instr? (first instrs))
               (let ([patched-instrs (patch-instr (first instrs))])
                 (loop (rest instrs)
                       (append (reverse patched-instrs) new-instrs)))]
              [(problematic-cmpq? (first instrs))
               (let ([patched-instrs (patch-problematic-cmpq (first instrs))])
                 (loop (rest instrs)
                       (append (reverse patched-instrs) new-instrs)))]
              [(problematic-leaq? (first instrs))
               (let ([patched-instrs (patch-problematic-leaq (first instrs))])
                 (loop (rest instrs)
                       (append (reverse patched-instrs) new-instrs)))]
              [(problematic-tailjmp? (first instrs))
               (let ([patched-instrs (patch-problematic-tailjmp
                                      (first instrs))])
                 (loop (rest instrs)
                       (append (reverse patched-instrs) new-instrs)))]
              [else (loop (rest instrs) (cons (first instrs) new-instrs))]))))]
    [else (error "patch")]))

(define (patch-def def)
  (match def
    [(Def name '() rty info label&blocks)
     (Def name
          '()
          rty
          info
          (for/list ([label&block label&blocks])
            (cons (car label&block) (patch (cdr label&block)))))]))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info defs) (X86Program info (map patch-def defs))]))

(define (prelude used-callee frame-sub root-stack-size name)

  (Block '()
         (append
          (list (Instr 'pushq (list (Reg 'rbp)))
                (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
          (append
           (map (lambda (reg) (Instr 'pushq (list (Reg reg)))) used-callee)
           (if (eqv? name 'main)
               (list (Instr 'subq (list (Imm frame-sub) (Reg 'rsp)))
                     (Instr 'movq (list (Imm 65536) (Reg 'rdi)))
                     (Instr 'movq (list (Imm 65536) (Reg 'rsi)))
                     (Callq 'initialize 2)
                     (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15))))
               (list (Instr 'subq (list (Imm frame-sub) (Reg 'rsp)))))
           (for/list ([i (range 0 root-stack-size)])
             (Instr 'movq (list (Imm 0) (Deref 'r15 i))))
           (list (Instr 'addq (list (Imm root-stack-size) (Reg 'r15)))
                 (Jmp (symbol-append name 'start)))))))

(define (conclusion used-callee frame-sub root-stack-size)
  (Block '()
         (cons (Instr 'subq (list (Imm root-stack-size) (Reg 'r15)))
               (cons (Instr 'addq (list (Imm frame-sub) (Reg 'rsp)))
                     (append (map (lambda (reg) (Instr 'popq (list (Reg reg))))
                                  used-callee)
                             (list (Instr 'popq (list (Reg 'rbp))) (Retq)))))))

(define (replace-tailjmp-instr instr used-callee frame-sub root-stack-size)
  (match instr
    [(TailJmp target n)
     (append (list (Instr 'subq (list (Imm root-stack-size) (Reg 'r15)))
                   (Instr 'addq (list (Imm frame-sub) (Reg 'rsp))))
             (map (lambda (reg) (Instr 'popq (list (Reg reg)))) used-callee)
             (list (Instr 'popq (list (Reg 'rbp))) (IndirectJmp target)))]
    [else instr]))

(define (replace-tailcall-block block used-callee frame-sub root-stack-size)
  (match block
    [(Block info instrs)
     (Block info
            (flatten (map (lambda (instr)
                            (replace-tailjmp-instr instr
                                                   used-callee
                                                   frame-sub
                                                   root-stack-size))
                          instrs)))]))

(define (replace-tailcall-blocks label&blocks
                                 used-callee
                                 frame-sub
                                 root-stack-size)
  (for/list ([label (map car label&blocks)] [block (map cdr label&blocks)])
    (cons
     label
     (replace-tailcall-block block used-callee frame-sub root-stack-size))))

(define (prefix-underscore label)
  (string->symbol (string-append "_" (symbol->string label))))

(define (if-macosx-instr instr)
  (match instr
    [(Jmp label) (Jmp (prefix-underscore label))]
    [(Callq label n) (Callq (prefix-underscore label) n)]
    [else instr]))

(define (if-macosx-block block)
  (match block
    [(Block '() instrs) (Block '() (map if-macosx-instr instrs))]))

(define (if-macosx p)
  (if (eqv? (system-type 'os) 'macosx)
      (match p
        [(X86Program info label&blocks)
         (X86Program info
                     (for/list ([label&block label&blocks])
                       (let ([label (car label&block)]
                             [block (cdr label&block)])
                         (let ([new-label (prefix-underscore label)])
                           (cons new-label (if-macosx-block block))))))])
      p))
;; prelude-and-conclusion : x86 -> x86
; (define (prelude-and-conclusion p)
;   (match p
;     [(X86Program info label&blocks)
;      (let ([start-space (dict-ref (dict-ref info 'stack-space) 'start)])
;        (if-macosx (X86Program info
;                               (cons (cons 'main (prelude start-space))
;                                     (append label&blocks
;                                             (list (cons 'conclusion (conclusion start-space))))))))]))

(define (re-offset-instr instr used-callee-space)

  (define (re-offset-arg arg)
    (match arg
      [(Deref reg offset) (Deref reg (- offset used-callee-space))]
      [else arg]))

  (match instr
    [(Instr name args) (Instr name (map re-offset-arg args))]
    [else instr]))

(define (re-offset-start label&blocks used-callee-space)
  (let ([labels (map car label&blocks)] [blocks (map cdr label&blocks)])
    (for/list ([label labels] [block blocks])
      (if (eqv? label 'start)
          (cons label
                (match block
                  [(Block info instrs)
                   (Block info
                          (map (lambda (instr)
                                 (re-offset-instr instr used-callee-space))
                               instrs))]))
          (cons label block)))))

; (define (prelude-and-conclusion p)
;   (match p
;     [(X86Program info label&blocks)
;      (let ([used-callee (dict-ref info 'used-callee)]
;            [spill-space (dict-ref info 'spill-space)]
;            [root-stack-space (* 8 (dict-ref info 'root-stack-size))])
;        (let ([used-callee-space (* 8 (length used-callee))])
;          (let ([frame-sub (- (align16 (+ used-callee-space spill-space))
;                              used-callee-space)])
;            (if-macosx
;             (X86Program
;              info
;              (cons (cons 'main (prelude used-callee frame-sub root-stack-space))
;                    (append (re-offset-start label&blocks used-callee-space)
;                            (list (cons 'conclusion
;                                        (conclusion used-callee
;                                                    frame-sub
;                                                    root-stack-space))))))))))]))

(define (prelude-and-conclusion-def def)
  (match def
    [(Def name '() rty info label&blocks)
     (let ([used-callee (dict-ref info 'used-callee)]
           [spill-space (dict-ref info 'spill-space)]
           [root-stack-space (dict-ref info 'root-stack-size)])

       ;  [root-stack-space (* 8 (dict-ref info 'root-stack-size))])
       (let ([used-callee-space (* 8 (length used-callee))])
         (let ([frame-sub (- (align16 (+ used-callee-space spill-space))
                             used-callee-space)])

           (cons
            (cons name (prelude used-callee frame-sub root-stack-space name))
            (append (replace-tailcall-blocks (re-offset-start label&blocks
                                                              used-callee-space)
                                             used-callee
                                             frame-sub
                                             root-stack-space)
                    (list (cons (symbol-append name 'conclusion)
                                (conclusion used-callee
                                            frame-sub
                                            root-stack-space))))))))]))

(define (prelude-and-conclusion p)
  (match p
    [(X86Program info defs)
     (X86Program info
                 (for/fold ([label&blocks '()]) ([def defs])
                   (append (prelude-and-conclusion-def def) label&blocks)))]))

; (define (pe-exp-lvar env e)
;   (match e
;     [(Var x) (dict-ref env x)]
;     [(Int n) (Int n)]
;     [(Let x rhs body)
;      (let ([e-val (pe-exp-lvar env rhs)])
;        (if (Int? e-val)
;            (let ([new-env (cons (cons x e-val) env)])
;              (pe-exp-lvar new-env body))
;            (Let x e-val (pe-exp-lvar env body))))]
;     [(Prim 'read '()) (Prim 'read '())]
;     [(Prim '- (list rand))
;      (let ([rand-val (pe-exp-lvar env rand)])
;        (pe-neg rand-val))]
;     [(Prim '- (list rand1 rand2))
;      (let ([rand1-val (pe-exp-lvar env rand1)]
;            [rand2-val (pe-exp-lvar env rand2)])
;        (pe-sub rand1-val rand2-val))]
;     [(Prim '+ (list rand1 rand2))
;      (let ([rand1-val (pe-exp-lvar env rand1)]
;            [rand2-val (pe-exp-lvar env rand2)])
;        (pe-add rand1-val rand2-val))]))

(define (pe-neg-res env r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [(Var x) (Int (fx- 0 (dict-ref env x)))]
    [else (Prim '- (list r))]))

(define (pe-add-res env r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [((Int n) (Var x)) (Int (fx+ (dict-ref env x) n))]
    [((Int n) (Prim '- (Var x))) (Int (fx- (dict-ref env x) n))]
    [((Int n1) (Prim '+ (list (Int n2) inert)))
     (Prim '+ (list (Int (fx+ n1 n2)) inert))]
    [(inert (Int n)) (pe-add-res env (Int n) inert)]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (add-res-trans e)
  (match e
    [(Prim '+ (list (Int n1) r2)) (Prim '+ (list (Int n1) (add-res-trans r2)))]
    [(Prim '+ (list r1 (Int n2))) (Prim '+ (list (Int n2) (add-res-trans r1)))]
    [(Prim '+ (list r1 r2))
     (Prim '+ (list (add-res-trans r1) (add-res-trans r2)))]
    [else e]))

(define (pe-sub-res env r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [((Var x1) (Int n2)) (Int (fx- (dict-ref env x1) n2))]
    [((Int n1) (Var x2)) (Int (fx- n1 (dict-ref env x2)))]
    [((Var x1) (Var x2)) (Int (fx- (dict-ref env x1) (dict-ref env x2)))]
    [(_ _) (Prim '- (list r1 r2))]))

(define (pe-exp-lvar env e)
  (match e
    [(Var x) (dict-ref env x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     (let ([e-val (pe-exp-lvar env rhs)])
       (if (Int? e-val)
           (let ([new-env (cons (cons x e-val) env)])
             (pe-exp-lvar new-env body))
           (Let x e-val (pe-exp-lvar env body))))]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list rand))
     (let ([rand-val (pe-exp-lvar env rand)]) (pe-neg-res env rand-val))]
    [(Prim '- (list rand1 rand2))
     (let ([rand1-val (pe-exp-lvar env rand1)]
           [rand2-val (pe-exp-lvar env rand2)])
       (pe-sub-res env rand1-val rand2-val))]
    [(Prim '+ (list rand1 rand2))
     (let ([rand1-val (pe-exp-lvar env rand1)]
           [rand2-val (pe-exp-lvar env rand2)])
       (pe-add-res env rand1-val rand2-val))]))

(define (pe-Lvar p)
  (match p
    [(Program '() e) (Program '() (pe-exp-lvar '() e))]))

(define caller-saved '(rax rcx rdx rsi rdi r8 r9 r10 r11))

(define callee-saved '(rsp rbp rbx r12 r13 r14 r15))

(define argument-pass '(rdi rsi rdx rcx r8 r9)) ;;; all of them are caller saved

(define label->live 'UNINITIATED)

(define (init-label->live!)
  (set! label->live '()))

(define conclusion-live (set 'rax 'rsp))

(define (update!-label->live label lives)
  (let ([new-label->live (dict-set label->live label lives)])
    (set! label->live new-label->live)))

(define blocks-with-lives 'UNINITIATED)

(define (init!-blocks-with-lives blocks)
  (set! blocks-with-lives blocks))

(define (update!-blocks-with-lives label block)
  (let ([new-blocks (dict-set blocks-with-lives label block)])
    (set! blocks-with-lives new-blocks)))

(define (live-arg arg)
  (match arg
    [(Imm n) (set)]
    [(Reg reg) (set reg)]
    [(Deref reg int) (set reg)]
    [(Var x) (set x)]
    [(Bool n) (set)]
    [(ByteReg reg) (set reg)]
    [(Global label) (set)]))

(define (read-instr instr)
  (match instr
    [(Instr 'addq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'subq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'negq (list arg1)) (live-arg arg1)]
    [(Instr 'movq (list arg1 arg2)) (live-arg arg1)]
    [(Instr 'xorq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'orq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'andq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'imulq (list arg1 arg2))
     (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'sarq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'salq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'cmpq (list arg1 arg2)) (set-union (live-arg arg1) (live-arg arg2))]
    [(Instr 'set (list cc arg2)) (set)]
    [(Instr 'movzbq (list arg1 arg2)) (live-arg arg1)]
    [(Instr 'pushq (list arg1)) (live-arg arg1)]
    [(Instr 'popq (list arg1)) (set)]
    [(Callq label n) (list->set (take argument-pass n))]
    [(Jmp label) (set)]
    [(JmpIf cc label) (set)]
    [(IndirectCallq label n)
     (set-union (live-arg label) (list->set (take argument-pass n)))]
    [(TailJmp label n)
     (set-union (live-arg label) (list->set (take argument-pass n)))]
    [(Instr 'leaq (list arg1 arg2)) (live-arg arg1)]))

(define (write-instr instr)
  (match instr
    [(Instr 'addq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'subq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'negq (list arg1)) (live-arg arg1)]
    [(Instr 'movq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'xorq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'orq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'andq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'imulq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'sarq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'salq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'cmpq (list arg1 arg2)) (set)]
    [(Instr 'set (list cc arg2)) (live-arg arg2)]
    [(Instr 'movzbq (list arg1 arg2)) (live-arg arg2)]
    [(Instr 'pushq (list arg1)) (set)]
    [(Instr 'popq (list arg1)) (live-arg arg1)]
    [(Callq label n) (list->set caller-save)]
    [(Jmp label) (set)]
    [(JmpIf cc label) (set)]
    [(IndirectCallq label n) (list->set caller-save)]
    [(TailJmp label n) (list->set caller-save)]
    [(Instr 'leaq (list arg1 arg2)) (live-arg arg2)]))

; (define (live-instr instr live-after)
;   (let ([reads (read-instr instr)]
;         [writes (write-instr instr)])
;     (set-union (set-subtract live-after writes) reads)))

(define (live-instr instr pre live-after sym->label)
  (match instr
    [(Jmp label) (set-union (dict-ref label->live label (set)) live-after)]
    [(JmpIf cc label)
     (begin
       (assert #t (Jmp? pre))
       (set-union (dict-ref label->live label (set)) live-after))]
    [(IndirectCallq (Var sym) n)
     (let ([reads (read-instr instr)] [writes (write-instr instr)])
       (set-union (dict-ref label->live (dict-ref sym->label sym #f) (set))
                  (set-union (set-union (set-subtract live-after writes) reads)
                             live-after)))]
    [(TailJmp (Var sym) n)
     (let ([reads (read-instr instr)] [writes (write-instr instr)])
       (set-union (dict-ref label->live (dict-ref sym->label sym #f) (set))
                  (set-union (set-union (set-subtract live-after writes) reads)
                             live-after)))]
    [else
     (let ([reads (read-instr instr)] [writes (write-instr instr)])
       (set-union (set-subtract live-after writes) reads))]))

; (define (live-block label block)
;   (match block
;     [(Block info instrs)
;      (let loop ([instrs-rev (reverse instrs)] [lives '()] [live-after (set)] [pre #f])
;        (if (null? instrs-rev)
;            (begin
;              (set! label->live (cons (cons label live-after) label->live))
;              (Block (append (list (cons 'lives lives)) info) instrs))
;            (let ([new-live-after (live-instr (first instrs-rev) pre live-after)])
;              (loop (rest instrs-rev)
;                    (cons new-live-after lives)
;                    new-live-after
;                    (first instrs-rev)))))]))

(define (live-block label block live-after sym->label)
  ; (debug-repl)
  (match block
    [(Block info instrs)
     (let loop ([instrs-rev (reverse instrs)]
                [lives '()]
                [live-after live-after]
                [pre #f])
       (if (null? instrs-rev)
           (begin
             (update!-label->live label live-after)
             (Block (dict-set info 'lives lives) instrs))
           (let ([new-live-after
                  (live-instr (first instrs-rev) pre live-after sym->label)])
             (loop (rest instrs-rev)
                   (cons new-live-after lives)
                   new-live-after
                   (first instrs-rev)))))]))

(define (build-cfg-body label&blocks)
  (define (init-cfg)
    (let ([cfg (make-multigraph '())])
      (let ([labels (map car label&blocks)])
        (begin
          (for ([label labels])
            (add-vertex! cfg label))
          cfg))))

  (define (successors-instr instr)
    (match instr
      [(Jmp label) label]
      [(JmpIf cc label) label]
      [else #f]))

  (define (successors block)
    (match block
      [(Block info instrs)
       (foldl
        (lambda (instr succs)
          (let ([succ (successors-instr instr)])
            (if (and succ (not (member succ succs))) (cons succ succs) succs)))
        '()
        instrs)]))

  (let ([cfg (init-cfg)])
    (let ([labels (map car label&blocks)] [blocks (map cdr label&blocks)])
      (begin
        (for ([label labels] [block blocks])
          (let ([succs (successors block)])
            (for ([succ succs])
              (add-directed-edge! cfg label succ))))
        cfg))))

(define (build-cfg p)
  (match p
    [(X86Program info body) (build-cfg-body body)]))

(define (analyze-dataflow G transfer bottom join)
  ;;; symbol -> live-set
  (define mapping (make-hash))
  (for ([v (in-vertices G)])
    (dict-set! mapping v bottom))
  (define worklist (make-queue))
  (for ([v (in-vertices G)])
    (enqueue! worklist v))
  (define trans-G (transpose G))
  (while (not (queue-empty? worklist))
         (define node (dequeue! worklist))
         (define input
           (for/fold ([state bottom]) ([pred (in-neighbors trans-G node)])
             (join state (dict-ref mapping pred))))
         (define output (transfer node input))
         (cond
           [(not (equal? output (dict-ref mapping node)))
            (dict-set! mapping node output)
            (for ([v (in-neighbors G node)])
              (enqueue! worklist v))]))
  mapping)

;;; symbol * live-after -> live-before
(define (make-transfer-block sym->label)
  (define (transfer-block label live-after)
    (let ([block (dict-ref blocks-with-lives label #f)])
      (if block
          (let ([new-block (live-block label block live-after sym->label)])
            (update!-blocks-with-lives label new-block)
            (let ([lives (dict-ref (Block-info new-block) 'lives)])
              (let ([res (if (null? lives) (set) (car lives))])
                (begin
                  (update!-label->live label res)
                  res))))
          (set))))
  transfer-block)

;;;x86-var -> x86-var
; (define (uncover-live p)
;   (init-label->live!)
;   (match p
;     [(X86Program info label&blocks)
;      (let ([labels (map car label&blocks)] [blocks (map cdr label&blocks)])
;        (X86Program
;         info
;         (let* ([cfg (build-cfg-body label&blocks)] [cfg^t (transpose cfg)] [order (tsort cfg^t)])
;           (begin
;             (assert #t (eqv? (first order) 'conclusion))
;             (for/list ([label (rest order)])
;               (let ([block (dict-ref label&blocks label)])
;                 (cons label (live-block label block))))))))]))

; (define (uncover-live p)
;   (init-label->live!)
;   (match p
;     [(X86Program info label&blocks)
;      (let* ([cfg (build-cfg-body label&blocks)] [cfg^t (transpose cfg)])
;        (init!-blocks-with-lives label&blocks)
;        (analyze-dataflow cfg^t transfer-block (set) set-union)
;        (X86Program info blocks-with-lives))]))

(define (uncover-live-def def)
  (match def
    [(Def label '() rty info label&blocks)
     (begin
       (let* ([cfg (build-cfg-body label&blocks)] [cfg^t (transpose cfg)])
         (init!-blocks-with-lives label&blocks)
         (analyze-dataflow cfg^t
                           (make-transfer-block (dict-ref info 'sym->label))
                           (set)
                           set-union)
         (Def label '() rty info blocks-with-lives)))]))

(define (uncover-live p)
  (match p
    [(X86Program info defs)
     (begin
       (init-label->live!)
       (for/fold ([_ (void)]) ([def defs])
         (update!-label->live (symbol-append (Def-name def) 'conclusion)
                              conclusion-live))
       (X86Program info (map uncover-live-def defs)))]))

(define (link g
              s
              d)
  (if (and (symbol? s) (symbol? d))
      (begin
        (add-edge! g s d)
        g)
      g))

(define (links g v vs)
  (let loop ([vs vs])
    (if (null? vs)
        g
        (begin
          (link g
                v
                (first vs))
          (loop (rest vs))))))

(define (mov-interfence s d after g)
  (let loop ([after after] [g g])
    (if (null? after)
        g
        (let ([v (first after)])
          (if (or (eqv? s v) (eqv? d v))
              (loop (rest after) g)
              (loop (rest after)
                    (link g
                          v
                          d))))))
  g)

(define (other-interfence writes after g)
  (if (null? writes)
      g
      (let ([d (first writes)])
        (let loop ([after-iter after] [g g])
          (if (null? after-iter)
              (other-interfence (cdr writes) after g)
              (let ([v (first after-iter)])
                (if (eqv? v d)
                    (loop (rest after-iter) g)
                    (loop (rest after-iter)
                          (link g
                                v
                                d)))))))))

(define (interfence-call-live-tuple-typed-vars after locals-types g)
  (define REG-FOR-ALLOCATE '(rcx rdx rsi rdi r8 r9 r10 rbx r12 r13 r14))
  ; (define REG-FOR-ALLOCATE (append callee-saved caller-saved))

  ; (displayln after)
  (let loop ([after after])
    (if (null? after)
        g
        (let ([live-var (first after)])
          (if (vector-type? (dict-ref locals-types live-var #f))
              (begin
                ;  (displayln live-var)
                (links g live-var REG-FOR-ALLOCATE)
                (loop (rest after)))
              (loop (rest after)))))))

(define (mov-arg-sym arg)
  (match arg
    [(Imm n) #f]
    [(Reg reg) reg]
    [(ByteReg reg) reg]
    [(Deref reg int) #f]
    [(Var x) x]
    [(Global label) #f]))

(define (live-instr-interfence after instr locals-types g)
  (match instr
    [(Instr 'movq (list arg1 arg2))
     (mov-interfence (mov-arg-sym arg1) (mov-arg-sym arg2) (set->list after) g)]
    [(Instr 'movzbq (list arg1 arg2))
     (mov-interfence (mov-arg-sym arg1) (mov-arg-sym arg2) (set->list after) g)]
    [(Callq label n)
     (interfence-call-live-tuple-typed-vars (set->list after) locals-types g)]
    [(TailJmp label n)
     (interfence-call-live-tuple-typed-vars (set->list after) locals-types g)]
    [(IndirectCallq label n)
     (interfence-call-live-tuple-typed-vars (set->list after) locals-types g)]
    [else
     (let ([writes (write-instr instr)])
       (other-interfence (set->list writes) (set->list after) g))]))

(define (block-interfence block g locals-types)
  (match block
    [(Block info instrs)
     (let ([lives (dict-ref info 'lives)])
       (let loop ([lives (rest lives)] [instrs instrs] [g g])
         (if (null? lives)
             g
             (loop (rest lives)
                   (rest instrs)
                   (live-instr-interfence (first lives)
                                          (first instrs)
                                          locals-types
                                          g)))))]))

;x86_var -> x86_var
; (define (build-interference p)
;   (match p
;     [(X86Program info label&blocks)
;      (let ([locals-types (dict-ref info 'locals-types)])
;        (let loop ([blocks (map cdr label&blocks)]
;                   [g (undirected-graph
;                       (append caller-saved callee-saved argument-pass))])
;          (if (null? blocks)
;              (let ([new-info (append (list (cons 'conflicts g)) info)])
;                (X86Program new-info label&blocks))
;              (loop (rest blocks)
;                    (block-interfence (first blocks) g locals-types)))))]))

(define (build-interference-def def)
  (match def
    [(Def label '() rty info label&blocks)
     (let ([locals-types (dict-ref info 'locals-types)])
       (let ([locals (map car locals-types)])
         ;  (displayln locals)
         (let loop ([blocks (map cdr label&blocks)]
                    [g
                     (undirected-graph
                      (append locals caller-saved callee-saved argument-pass))])
           (if (null? blocks)
               (let ([new-info (append (list (cons 'conflicts g)) info)])
                 (Def label '() rty new-info label&blocks))
               (loop (rest blocks)
                     (block-interfence (first blocks) g locals-types))))))]))

(define (build-interference p)
  (match p
    [(X86Program info defs)
     (X86Program info (map build-interference-def defs))]))

(define (graph-of-program-start p)
  (match p
    [(X86Program info body) (dict-ref (dict-ref info 'conflicts) 'start)]))

(struct Vertex (name color staturation) #:transparent #:mutable)

(define (put-staturation-vertex! vertex s)
  (match vertex
    [(Vertex color name staturation)
     (let ([new-staturation
            (if (member s staturation) staturation (cons s staturation))])
       (begin
         (set-Vertex-staturation! vertex new-staturation)
         vertex))]))

(define the-reg-color-map
  '((rcx . 0) (rdx . 1)
              (rsi . 2)
              (rdi . 3)
              (r8 . 4)
              (r9 . 5)
              (r10 . 6)
              (rbx . 7)
              (r12 . 8)
              (r13 . 9)
              (r14 . 10)
              (rax . -1)
              (rsp . -2)
              (rbp . -3)
              (r11 . -4)
              (r15 . -5))) ; r15 is used by root stack

;;; for test, page 50
; (define the-reg-color-map
;   '((rcx . 0) (rbx . 1)
;               (rax . -1) (rsp . -2) (rbp . -3) (r11 . -4) (r15 . -5)))

(define (reg-of-the-reg-color-map color)
  (let loop ([reg-color-map the-reg-color-map])
    (if (null? reg-color-map)
        (error 'reg-of-the-reg-color-map)
        (let ([reg (car (first reg-color-map))]
              [reg-color (cdr (first reg-color-map))])
          (if (= reg-color color) reg (loop (rest reg-color-map)))))))

(define (every? pred lst)
  (if (null? lst) #t (and (pred (first lst)) (every? pred (rest lst)))))

(define (color-graph g vars def)
  (define NOCOLOR '-)

  (define (make-cmp move-related-graph)
    (lambda (v1 v2)
      (let ([len1 (length (Vertex-staturation v1))]
            [len2 (length (Vertex-staturation v2))])
        (if (= len1 len2)
            (let ([name1 (Vertex-name v1)] [name2 (Vertex-name v2)])
              (let ([neighbors-move-related-v1
                     (if (has-vertex? move-related-graph name1)
                         (sequence->list (in-neighbors move-related-graph
                                                       name1))
                         '())]
                    [neighbors-move-related-v2
                     (if (has-vertex? move-related-graph name2)
                         (sequence->list (in-neighbors move-related-graph
                                                       name2))
                         '())])
                (let ([related-len1 (length neighbors-move-related-v1)]
                      [related-len2 (length neighbors-move-related-v2)])
                  (if (= related-len1 related-len2)
                      (symbol<? name1 name2) ; order in text book
                      (> related-len1 related-len2)))))
            (> len1 len2)))))

  (define (build-move-related-graph)
    (define (loop-block-instrs block)
      (let loop ([edges '()] [instrs (Block-instr* block)]) ; todo
        (if (null? instrs)
            edges
            (let ([instr (first instrs)])
              (match instr
                [(Instr 'movq (list (Var name1) (Var name2)))
                 (loop (cons (list name1 name2) edges) (rest instrs))]
                [else (loop edges (rest instrs))])))))
    (match def
      [(Def name '() rty info body)
       (undirected-graph (flatten (map loop-block-instrs (map cdr body))))]))

  (define (update-queue! que handle-map vertex)
    (let ([name (Vertex-name vertex)])
      (let ([handle (dict-ref handle-map name #f)])
        (if handle (pqueue-decrease-key! que handle) 'void))))

  (define (fill-staturation-vertex! vertex-sym
                                    neighbors
                                    vertex-map
                                    handle-map
                                    que)
    (let ([vertex (dict-ref vertex-map vertex-sym)])
      (let loop ([neighbors neighbors])
        (if (null? neighbors)
            'void
            (let ([neighbor (first neighbors)])
              (let ([neighbor-vertex (dict-ref vertex-map neighbor)])
                (let ([neighbor-color (Vertex-color neighbor-vertex)])
                  (if (eqv? neighbor-color NOCOLOR)
                      (loop (rest neighbors))
                      (begin
                        (put-staturation-vertex! vertex neighbor-color)
                        (update-queue! que handle-map vertex)
                        (loop (rest neighbors)))))))))))

  (define (fill-staturation-graph! g vars vertex-map handle-map que)
    (for/list ([var vars])
      (let ([neighbors (sequence->list (in-neighbors g var))])
        (fill-staturation-vertex! var neighbors vertex-map handle-map que))))

  (define (init!)
    (let ([move-related-graph (build-move-related-graph)])
      ; (displayln (graphviz move-related-graph))
      (let ([que (make-pqueue (make-cmp move-related-graph))]) ; 存放未着色节点
        (let loop ([vs vars] [vertex-map '()] [handle-map '()])
          (if (null? vs)
              (begin
                (fill-staturation-graph! g vars vertex-map handle-map que)
                (values que vertex-map handle-map move-related-graph))
              (let ([color (dict-ref the-reg-color-map (first vs) NOCOLOR)])
                (let ([vertex (Vertex (first vs) color '())])
                  (if (eqv? color NOCOLOR)
                      (let ([handle (pqueue-push! que vertex)])
                        (loop (rest vs)
                              (cons (cons (first vs) vertex) vertex-map)
                              (cons (cons (first vs) handle) handle-map)))
                      (loop (rest vs)
                            (cons (cons (first vs) vertex) vertex-map)
                            handle-map)))))))))

  (define (next-color v-name staturation move-related-graph vertex-map)
    (define (move-related-color)
      (let ([related (if (has-vertex? move-related-graph v-name)
                         (sequence->list (in-neighbors move-related-graph
                                                       v-name))
                         '())])
        (let loop ([related related])
          (if (null? related)
              #f
              (let ([related-vertex (dict-ref vertex-map (first related))])
                (let ([related-color (Vertex-color related-vertex)])
                  (if (eqv? related-color NOCOLOR)
                      (loop (rest related))
                      (if (member related-color staturation)
                          (loop (rest related))
                          related-color))))))))

    (let ([related-color (move-related-color)])
      (if related-color
          related-color
          (let loop ([color 0])
            (if (member color staturation) (loop (+ color 1)) color)))))

  (define (update-neighbors-staturation! vertex vertex-map handle-map que)
    ; (displayln vertex-map)
    (match vertex
      [(Vertex name color staturation)
       (let loop ([neighbors (sequence->list (in-neighbors g name))])
         (if (null? neighbors)
             'void
             (let ([neighbor (first neighbors)])
               (let ([neighbor-vertex (dict-ref vertex-map neighbor)])
                 (begin
                   (put-staturation-vertex! neighbor-vertex color)
                   (update-queue! que handle-map neighbor-vertex)
                   (loop (rest neighbors)))))))]))

  ; dsatur 算法：给某个结点染色，更新邻结点的saturation，更新后的邻结点可能会产生冲突吗？
  ; 不会，所以不需要这个方法
  ; (define (check-neighbors vertex vertex-map)
  ;   ; (displayln vertex-map)
  ;   (define (check-1 vertex) ; 1层neighbor检查
  ;     (match vertex
  ;       [(Vertex name color staturation)
  ;        (let loop ([neighbors (sequence->list (in-neighbors g name))])
  ;          (if (null? neighbors)
  ;              #t
  ;              (let ([neighbor-vertex (dict-ref vertex-map (first neighbors))])
  ;                (and (not (member (Vertex-color neighbor-vertex) (Vertex-staturation neighbor-vertex)))
  ;                     (loop (rest neighbors))))))]))
  ;   (let ([neighbors (sequence->list (in-neighbors g (Vertex-name vertex)))])
  ;     (every? check-1 (map (lambda (v) (dict-ref vertex-map v)) neighbors))))

  (define (dsatur-color! que vertex-map handle-map move-related-graph)
    (if (zero? (pqueue-count que))
        vertex-map
        (let ([vertex (pqueue-pop! que)])
          (match vertex
            [(Vertex name color staturation)
             (if (not (eqv? color NOCOLOR))
                 (error 'dsatur-color!)
                 (let ([new-color (next-color name
                                              staturation
                                              move-related-graph
                                              vertex-map)])
                   (begin
                     (set-Vertex-color! vertex new-color)
                     (update-neighbors-staturation! vertex
                                                    vertex-map
                                                    handle-map
                                                    que)
                     (dsatur-color!
                      que
                      vertex-map
                      handle-map
                      move-related-graph))))])))) ; what if fail to color graph?

  (define-values (que vertex-map handle-map move-related-graph) (init!))

  (let ([vertex-map
         (dsatur-color! que vertex-map handle-map move-related-graph)])
    (let loop ([vertex-map vertex-map] [color-map '()])
      (if (null? vertex-map)
          color-map
          (let ([var (car (first vertex-map))]
                [color (Vertex-color (cdr (first vertex-map)))])
            (loop (rest vertex-map) (cons (cons var color) color-map)))))))

; (define (vars-in-graph g)
;   (filter (lambda (v) (> (length (sequence->list (in-neighbors g v))) 0))
;           (sequence->list (in-vertices g))))

(define (vars-in-graph g)
  (sequence->list (in-vertices g)))

(define (allocate-registers p)
  (define REG-AVALIABLE 11)

  (define ROOT-STACK 'UNINITIATED)

  (define (init!-ROOT-STACK)
    (set! ROOT-STACK '()))

  (define (push!-ROOT-STACK var)
    (let ([size (length ROOT-STACK)])
      (begin
        (set! ROOT-STACK (cons (cons var size) ROOT-STACK))
        size)))

  (define (calc-root-stack-location var)
    (let ([localtion (dict-ref ROOT-STACK var #f)])
      (if localtion
          (Deref 'r15 localtion)
          (Deref 'r15 (push!-ROOT-STACK var)))))

  (define (build-var-map color-map locals-types reg-avaliable)
    ;  (debug-repl)
    (let loop ([color-map color-map] [var-map '()])
      (if (null? color-map)
          var-map
          (let ([var (car (first color-map))] [color (cdr (first color-map))])
            (let ([location (if (vector-type? (dict-ref locals-types var #f))
                                (calc-root-stack-location var)
                                (if (< color reg-avaliable)
                                    (Reg (reg-of-the-reg-color-map color))
                                    (let ([offset (* (- (+ color 1)
                                                        reg-avaliable)
                                                     -8)]) ; todo: other types
                                      (Deref 'rbp offset))))])
              (loop (rest color-map) (cons (cons var location) var-map)))))))

  (define (assign-arg arg var-map)
    ; (displayln arg)
    ; (displayln var-map)
    (match arg
      [(Var name) (dict-ref var-map name)]
      [(Imm n) (Imm n)]
      [(Reg name) (Reg name)]
      [(ByteReg name) (ByteReg name)]
      [(Deref reg offset) (Deref reg offset)]
      [(Global label) (Global label)]))

  (define (assign-instr instr var-map)
    (match instr
      [(Instr 'set (list cc arg))
       (Instr 'set (list cc (assign-arg arg var-map)))]
      [(Instr name args)
       (Instr name (map (lambda (arg) (assign-arg arg var-map)) args))]
      [(IndirectCallq arg n) (IndirectCallq (assign-arg arg var-map) n)]
      [(TailJmp arg n) (TailJmp (assign-arg arg var-map) n)]
      [else instr]))

  (define (assign-location block var-map)
    (match block
      [(Block info instrs)
       (Block info
              (map (lambda (instr) (assign-instr instr var-map)) instrs))]))

  (define (allocate-reg-body var-map body)
    (let ([labels (map car body)] [blocks (map cdr body)])
      (for/list ([label labels] [block blocks])
        (cons label (assign-location block var-map)))))

  (define (written-callee body)
    (define (callee-block block)
      (match block
        [(Block info instrs)
         (list->set
          (filter (lambda (reg)
                    (and (member reg callee-saved) (not (eqv? 'rbp reg))))
                  (set->list (foldl (lambda (writes res) (set-union writes res))
                                    (set)
                                    (map write-instr instrs)))))]))
    (set->list (foldl (lambda (block writes)
                        (set-union (callee-block block) writes))
                      (set)
                      (map cdr body))))

  (define (calc-spill-space var-map)
    (let loop ([space 0] [var-map var-map])
      (if (null? var-map)
          space
          (match (cdr (first var-map))
            [(Reg name) (loop space (rest var-map))]
            [(Deref name offset) (loop (max space (- offset)) (rest var-map))]
            [else (error "calc-spill-space: ~s" (first var-map))]))))

  (define (allocate-def def)
    (match def
      [(Def name '() rty info body)
       ;  (when (eqv? name 'main)
       ;    (debug-repl))
       (let* ([locals-types (dict-ref info 'locals-types)]
              [g (dict-ref info 'conflicts)]
              [vars (vars-in-graph g)]
              [color-map (color-graph g vars def)]
              [var-map (build-var-map color-map locals-types REG-AVALIABLE)]
              [new-body (allocate-reg-body var-map body)]
              [used-callee (written-callee new-body)]
              [spill-space (calc-spill-space var-map)])
         ;  (when (eqv? name 'main)
         ;        (begin
         ;         (displayln color-map)
         ;        (displayln var-map)))
         (Def name
              '()
              rty
              (append (list (cons 'spill-space spill-space))
                      (list (cons 'used-callee used-callee))
                      (list (cons 'root-stack-size (length ROOT-STACK)))
                      info)
              new-body))]))

  (match p
    [(X86Program info body)
     (init!-ROOT-STACK)
     (X86Program info (map allocate-def body))]))

;;; L -> L
(define (shrink-expr expr)
  (match expr
    [(Int n) (Int n)]
    [(Var sym) (Var sym)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Let var rhs body) (Let var (shrink-expr rhs) (shrink-expr body))]
    [(If cnd thn els)
     (If (shrink-expr cnd) (shrink-expr thn) (shrink-expr els))]
    [(Prim 'and (list e1 e2)) (If (shrink-expr e1) (shrink-expr e2) (Bool #f))]
    [(Prim 'or (list e1 e2)) (If (shrink-expr e1) (Bool #t) (shrink-expr e2))]
    [(Prim op args) (Prim op (map shrink-expr args))]
    [(SetBang var rhs) (SetBang var (shrink-expr rhs))]
    [(Begin exps body) (Begin (map shrink-expr exps) (shrink-expr body))]
    [(Apply f args) (Apply (shrink-expr f) (map shrink-expr args))]
    [(Lambda params rty body) (Lambda params rty (shrink-expr body))]
    [(WhileLoop cnd body) (WhileLoop (shrink-expr cnd) (shrink-expr body))]))

(define (shrink-def def)
  (match def
    [(Def name param* rty info body)
     (Def name param* rty info (shrink-expr body))]))

;;; L -> L
(define (shrink p)
  (match p
    [(ProgramDefsExp info defs exp)
     (ProgramDefs
      info
      (append (map shrink-def defs)
              (list (Def 'main '() 'Integer '() (shrink-expr exp)))))]))

(define (remove-jumps p)
  (define (basic-block? label)
    (not (or (eqv? label 'main) (eqv? label 'start) (eqv? label 'conclusion))))

  (define prunes 'UNINITIATED)

  (define (init-prunes!)
    (set! prunes '()))

  (define (replace-jmp instrs from from-label)
    (let loop ([instrs instrs] [new-instrs '()])
      (if (null? instrs)
          (reverse new-instrs)
          (match (first instrs)
            [(Jmp label)
             (if (eqv? label from-label)
                 (begin
                   (set! prunes (cons label prunes))
                   (loop (rest instrs) (append (reverse from) new-instrs)))
                 (loop (rest instrs) (cons (first instrs) new-instrs)))]
            [else (loop (rest instrs) (cons (first instrs) new-instrs))]))))

  (define (prune body)
    (filter (lambda (label&block)
              (let ([label (car label&block)]) (not (member label prunes))))
            body))

  (define (merge-block to from from-label)
    (match* (to from)
      [((Block info-to instrs-to) (Block info-from instrs-from))
       (Block '()
              (replace-jmp instrs-to
                           instrs-from
                           from-label))])) ; throw away block info

  (define (merge body order merge-dict)
    (let loop ([labels order] [new-body '()])
      (if (null? labels)
          new-body
          (let ([label (first labels)])
            (let ([block (dict-ref body label #f)])
              (if block
                  (let ([merge-from (dict-ref merge-dict label #f)])
                    (if merge-from
                        (let ([merged-block (merge-block block
                                                         (dict-ref new-body
                                                                   merge-from)
                                                         merge-from)])
                          (loop (rest labels)
                                (cons (cons label merged-block) new-body)))
                        (loop (rest labels)
                              (cons (cons label block) new-body))))
                  (loop (rest labels) new-body)))))))

  (let ([cfg^t (transpose (build-cfg p))])
    (let ([vertices (tsort cfg^t)])
      (let* ([v&neighss (map (lambda (v)
                               (cons v (sequence->list (in-neighbors cfg^t v))))
                             vertices)]
             [basics (filter (lambda (v&neighs)
                               (let ([v (car v&neighs)] [neighs (cdr v&neighs)])
                                 (and (basic-block? v) (= (length neighs) 1))))
                             v&neighss)]
             [merge-dict (map (lambda (v&neigh)
                                (cons (cadr v&neigh) ; merge to
                                      (car v&neigh))) ; merge from
                              basics)])
        (init-prunes!)
        (match p
          [(X86Program info body)
           (X86Program info (prune (merge body vertices merge-dict)))])))))

;;; typo : collect
(define (colloct-set! expr)
  (define (colloct exprs)
    (for/fold ([res (set)]) ([expr exprs])
      (set-union res (colloct-set! expr))))
  (match expr
    [(Var x) (set)]
    [(Int n) (set)]
    [(Bool b) (set)]
    [(Void) (set)]
    [(ValueOf e ftype) (colloct-set! e)]
    [(Exit) (set)]
    [(FunRef label n) (set)]
    [(Apply func args) (colloct (cons func args))]
    [(Lambda params rty body) (colloct (list body))]
    [(Let var rhs body) (colloct (list rhs body))]
    [(If cnd thn els) (colloct (list cnd thn els))]
    [(Prim op es) (colloct es)]
    [(SetBang var rhs) (set-union (set var) (colloct-set! rhs))]
    [(Begin exps body) (set-union (colloct exps) (colloct-set! body))]
    [(WhileLoop cnd body) (colloct (list cnd body))]
    [(Collect n) (set)]
    [(Allocate n ty) (set)]
    [(AllocateClosure len type arity) (set)]
    [(GlobalValue name) (set)]))

(define ((uncover-get!-expr set!-vars) expr)
  (define recur (uncover-get!-expr set!-vars))
  (match expr
    [(Var x) (if (set-member? set!-vars x) (GetBang x) (Var x))]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(ValueOf e ftype) (ValueOf (recur e) ftype)]
    [(Exit) (Exit)]
    [(Void) (Void)]
    [(FunRef label n) (FunRef label n)]
    [(Apply func args) (Apply (recur func) (map recur args))]
    [(Lambda params rty body) (Lambda params rty (recur body))]
    [(Let var rhs body) (Let var (recur rhs) (recur body))]
    [(If cnd thn els) (If (recur cnd) (recur thn) (recur els))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (recur e)))]
    [(SetBang var rhs) (SetBang var (recur rhs))]
    [(Begin exps body) (Begin (map recur exps) (recur body))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(Collect n) (Collect n)]
    [(Allocate n ty) (Allocate n ty)]
    [(AllocateClosure len type arity) (AllocateClosure len type arity)]
    [(GlobalValue name) (GlobalValue name)]))

(define (uncover-def def)
  (match def
    [(Def name param* rty info body)
     (let ([set!-vars (colloct-set! body)])
       (Def name param* rty info ((uncover-get!-expr set!-vars) body)))]))

(define (uncover-get! p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map uncover-def defs))]))

; set -> set
(define ((free-variables bounds) expr)
  (define recur (free-variables bounds))
  (define (union-exprs exprs)
    (for/fold ([res (set)]) ([expr exprs])
      (set-union (recur expr) res)))

  (match expr
    [(Var x) (if (set-member? bounds x) (set) (set x))]
    [(Int n) (set)]
    [(Bool b) (set)]
    [(Void) (set)]
    [(ValueOf e ftype) (recur e)]
    [(Exit) (set)]
    [(FunRef label n) (if (set-member? bounds label) (set) (set label))]
    [(Apply func args) (union-exprs (cons func args))]
    [(Lambda params rty body)
     (let ([new-bounds (set-union bounds (list->set (map param-name params)))])
       ((free-variables new-bounds) body))]
    [(Let var rhs body)
     (let ([new-bounds (set-union bounds (set var))])
       (set-union ((free-variables new-bounds) body)
                  ((free-variables bounds) rhs)))]
    [(If cnd thn els) (union-exprs (list cnd thn els))]
    [(Prim op es) (union-exprs es)]
    [(SetBang var rhs) (recur rhs)]
    [(Begin exps body) (union-exprs (reverse (cons body exps)))]
    [(WhileLoop cnd body) (union-exprs (list cnd body))]
    [(Collect n) (set)]
    [(Allocate n ty) (set)]
    [(GlobalValue name) (set)]))

(define (free-in-lambda expr)
  (define (union-exprs exprs)
    (for/fold ([res (set)]) ([expr exprs])
      (set-union (free-in-lambda expr) res)))

  (match expr
    [(Var x) (set)]
    [(Int n) (set)]
    [(Bool b) (set)]
    [(Void) (set)]
    [(FunRef label n) (set)]
    [(ValueOf e ftype) (free-in-lambda e)]
    [(Exit) (set)]
    [(Apply func args) (union-exprs (cons func args))]
    [(Lambda params rty body)
     (let ([bounds (list->set (map param-name params))])
       ((free-variables bounds) body))]
    [(Let var rhs body) (union-exprs (list rhs body))]
    [(If cnd thn els) (union-exprs (list cnd thn els))]
    [(Prim op es) (union-exprs es)]
    [(SetBang var rhs) (free-in-lambda rhs)]
    [(Begin exps body) (union-exprs (reverse (cons body exps)))]
    [(WhileLoop cnd body) (union-exprs (list cnd body))]
    [(Collect n) (set)]
    [(Allocate n ty) (set)]
    [(GlobalValue name) (set)]))

;  set of variables that are free in a lambda and
;  that are assigned to in the enclosing function definition.
;  expr -> set
(define (free-and-assigned expr)
  (let ([frees (free-in-lambda expr)] [assigned (colloct-set! expr)])
    (set-intersect frees assigned)))

(define ((convert-assignment-expr af-set) expr)
  (define recur (convert-assignment-expr af-set))
  (match expr
    [(Var x)
     (if (set-member? af-set x)
         (Prim 'vector-ref (list (Var x) (Int 0)))
         (Var x))]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(ValueOf e ftype) (ValueOf (recur e) ftype)]
    [(Exit) (Exit)]
    [(Void) (Void)]
    [(FunRef label n) (FunRef label n)]
    [(Apply func args) (Apply (recur func) (map recur args))]
    [(Lambda params rty body) (Lambda params rty (recur body))]
    [(Let var rhs body) (Let var (recur rhs) (recur body))]
    [(If cnd thn els) (If (recur cnd) (recur thn) (recur els))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (recur e)))]
    [(SetBang var rhs)
     (if (set-member? af-set var)
         (Prim 'vector-set! (list (Var var) (Int 0) (recur rhs)))
         (SetBang var rhs))]
    [(Begin exps body) (Begin (map recur exps) (recur body))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(Collect n) (Collect n)]
    [(Allocate n ty) (Allocate n ty)]
    [(GlobalValue name) (GlobalValue name)]))

(define (convert-assignment-def def)
  (match def
    [(Def name params rty info body)
     (let ([af-set (free-and-assigned body)]
           [param-names (map param-name params)])
       (let-values
           ([(box-param-names new-params env)
             (for/fold ([box-names '()] [new-params '()] [env '()])
                       ([param params])
               (if (set-member? af-set (param-name param))
                   (let* ([new-param-name (gen-sym (param-name param))]
                          [new-env
                           (dict-set env (param-name param) new-param-name)])
                     (values (cons (param-name param) box-names)
                             (cons (list new-param-name ': (param-type param))
                                   new-params)
                             new-env))
                   (values box-names (cons param new-params) env)))])
         (let ([new-params^ (reverse new-params)]
               [box-param-names^ (reverse box-param-names)])
           (Def
            name
            new-params^
            rty
            info
            (let loop ([box-param-names^^ box-param-names^])
              (if (null? box-param-names^^)
                  ((convert-assignment-expr af-set) body)
                  (Let (first box-param-names^^)
                       (Prim 'vector
                             (list (Var (dict-ref env
                                                  (first box-param-names^^)))))
                       (loop (rest box-param-names^^)))))))))]))

(define (convert-assignment p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map convert-assignment-def defs))]))

(define (expose-expr expr)
  (match expr
    [(HasType (Prim 'vector es) ts)
     (define size (type-space ts))
     (let loop-es ([es^ es] [vars '()])
       (if (null? es^)
           (Let
            (gen-sym '_)
            (If (Prim '<
                      (list (Prim '+ (list (GlobalValue 'free_ptr) (Int size)))
                            (GlobalValue 'fromspace_end)))
                (Void)
                (Collect size))
            (let ([vec-name (gen-sym 'v)] [len (length es)])
              (Let vec-name
                   (Allocate len ts) ;;; why need length of the vector?
                   (let loop-vars ([vars^ (reverse vars)] [i 0])
                     (if (null? vars^)
                         (Var vec-name)
                         (Let (gen-sym '_)
                              (Prim 'vector-set!
                                    (list (Var vec-name) (Int i) (first vars^)))
                              (loop-vars (rest vars^) (+ i 1))))))))
           (if (HasType? (first es^))
               (let ([var (gen-sym 'var)])
                 (Let var
                      (expose-expr (first es^))
                      (loop-es (rest es^) (cons (Var var) vars))))
               (loop-es (rest es^) (cons (first es^) vars)))))]
    [(HasType (Closure arity fvs) ts)
     (define size (type-space ts))
     (let ([es (cdr fvs)])
       (let loop-es ([es^ es] [vars '()])
         (if (null? es^)
             (Let (gen-sym '_)
                  (If (Prim
                       '<
                       (list (Prim '+ (list (GlobalValue 'free_ptr) (Int size)))
                             (GlobalValue 'fromspace_end)))
                      (Void)
                      (Collect size))
                  (let ([vec-name (gen-sym 'v)] [len (length es)])
                    (Let vec-name
                         (AllocateClosure len ts arity)
                         (let loop-vars ([vars^ (reverse vars)] [i 1])
                           (if (null? vars^)
                               (Var vec-name)
                               (Let (gen-sym '_)
                                    (Prim 'vector-set!
                                          (list (Var vec-name)
                                                (Int i)
                                                (first vars^)))
                                    (loop-vars (rest vars^) (+ i 1))))))))
             (if (HasType? (first es^))
                 (let ([var (gen-sym 'var)])
                   (Let var
                        (expose-expr (first es^))
                        (loop-es (rest es^) (cons (Var var) vars))))
                 (loop-es (rest es^) (cons (first es^) vars))))))]
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(ValueOf e ftype) (ValueOf (expose-expr e) ftype)]
    [(Exit) (Exit)]
    [(FunRef label n) (FunRef label n)]
    [(Apply func args) (Apply (expose-expr func) (map expose-expr args))]
    [(Let var rhs body) (Let var (expose-expr rhs) (expose-expr body))]
    [(If cnd thn els)
     (If (expose-expr cnd) (expose-expr thn) (expose-expr els))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (expose-expr e)))]
    [(SetBang var rhs) (SetBang var (expose-expr rhs))]
    [(Begin exps body) (Begin (map expose-expr exps) (expose-expr body))]
    [(WhileLoop cnd body) (WhileLoop (expose-expr cnd) (expose-expr body))]))

(define (expose-def def)
  (match def
    [(Def name param* rty info body)
     (Def name param* rty info (expose-expr body))]))

(define (expose-allocation p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map expose-def defs))]))

(define (func-param? param)
  (match param
    [(list name ': tys) (if (list? tys) ((length tys) . - . 2) #f)]
    [else #f]))

;;; dict(sym -> int) -> expr
(define ((reveal-exp f-names) expr)
  (match expr
    [(Int n) (Int n)]
    [(Var sym)
     (let ([param-len (dict-ref f-names sym #f)])
       (if param-len (FunRef sym param-len) (Var sym)))]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Let var rhs body)
     (Let var
          ((reveal-exp f-names) rhs)
          (if (Lambda? rhs)
              ((reveal-exp (dict-set f-names var (length (Lambda-param* rhs))))
               body)
              ((reveal-exp f-names) body)))]
    [(If cnd thn els)
     (If ((reveal-exp f-names) cnd)
         ((reveal-exp f-names) thn)
         ((reveal-exp f-names) els))]
    [(Prim op args) (Prim op (map (reveal-exp f-names) args))]
    [(SetBang var rhs) (SetBang var ((reveal-exp f-names) rhs))]
    [(Begin exps body)
     (Begin (map (reveal-exp f-names) exps) ((reveal-exp f-names) body))]
    [(Apply f args)
     (Apply ((reveal-exp f-names) f) (map (reveal-exp f-names) args))]
    [(Lambda params rty body)
     (let ([new-f-names
            (for/fold ([names f-names]) ([param params])
              (let ([param-len (func-param? param)])
                (if param-len (dict-set names (car param) param-len) names)))])
       (Lambda params rty ((reveal-exp new-f-names) body)))]
    [(WhileLoop cnd body)
     (WhileLoop ((reveal-exp f-names) cnd) ((reveal-exp f-names) body))]))

(define ((reveal-def f-names) def)

  (match def
    [(Def name param* rty info body)
     (let ([new-f-names
            (for/fold ([names f-names]) ([param param*])
              (let ([param-len (func-param? param)])
                (if param-len (dict-set names (car param) param-len) names)))])
       ;  (displayln new-f-names)
       (Def name param* rty info ((reveal-exp new-f-names) body)))]))

; L -> L
(define (reveal-functions p)
  (match p
    [(ProgramDefs info defs)
     (let ([f-names
            (for/fold ([dict '()]) ([def defs])
              (dict-set dict (Def-name def) (length (Def-param* def))))])
       (ProgramDefs info (map (reveal-def f-names) defs)))]))

(define (param-name param)
  (if (list? param) (car param) param)) ;;; dynamic typing

(define (param-type param)
  (if (list? param) (caddr param) '())) ;;; dynamic typing

(define (index-of lst ele)
  (let loop ([lst lst] [idx 0])
    (cond
      [(empty? lst) #f]
      [(equal? (first lst) ele) idx]
      [else (loop (rest lst) (+ 1 idx))])))

(define ((limit-func-expr tup-name spill-args) expr)
  (match expr
    [(Int n) (Int n)]
    [(ValueOf e ftype)
     (ValueOf ((limit-func-expr tup-name spill-args) e) ftype)]
    [(Exit) (Exit)]
    [(FunRef sym n)
     (let ([index (index-of spill-args sym)])
       (if index
           (Prim 'vector-ref (list (Var tup-name) (Int index)))
           (FunRef sym n)))]
    [(Closure arity fvs)
     (let ([sym (FunRef-name (first fvs))])
       (let ([index (index-of spill-args sym)])
         (if index
             (Prim 'vector-ref (list (Var tup-name) (Int index)))
             (Closure arity fvs))))]
    [(HasType (Closure arity fvs) ty)
     (let ([sym (FunRef-name (first fvs))])
       (let ([index (index-of spill-args sym)])
         (if index
             (Prim 'vector-ref (list (Var tup-name) (Int index)))
             (Closure arity fvs))))]
    [(Var sym)
     (let ([index (index-of spill-args sym)])
       (if index
           (Prim 'vector-ref (list (Var tup-name) (Int index)))
           (Var sym)))]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Let var rhs body)
     (Let var
          ((limit-func-expr tup-name spill-args) rhs)
          ((limit-func-expr tup-name spill-args) body))]
    [(If cnd thn els)
     (If ((limit-func-expr tup-name spill-args) cnd)
         ((limit-func-expr tup-name spill-args) thn)
         ((limit-func-expr tup-name spill-args) els))]
    [(Prim op args) (Prim op (map (limit-func-expr tup-name spill-args) args))]
    [(SetBang var rhs)
     (SetBang var ((limit-func-expr tup-name spill-args) rhs))]
    [(Begin exps body)
     (Begin (map (limit-func-expr tup-name spill-args) exps)
            ((limit-func-expr tup-name spill-args) body))]
    [(Apply f args)
     ;  (displayln args)
     (if (<= (length args) 6)
         (Apply ((limit-func-expr tup-name spill-args) f)
                (map (limit-func-expr tup-name spill-args) args))
         (let ([first-5 (take args 5)]
               [last-arg (Prim 'vector (list-tail args 5))])
           (let ([new-args (append first-5 (list last-arg))])
             (Apply ((limit-func-expr tup-name spill-args) f)
                    (map (limit-func-expr tup-name spill-args) new-args)))))]
    [(WhileLoop cnd body)
     (WhileLoop ((limit-func-expr tup-name spill-args) cnd)
                ((limit-func-expr tup-name spill-args) body))]))

(define (limit-func-def def)
  (match def
    [(Def name param* rty info body)
     (if (<= (length param*) 6)
         (Def name param* rty info ((limit-func-expr #f '()) body))
         (let-values
             ([(total rest-args-names rest-args-tys)
               (for/fold ([i 1] [rest-args-names '()] [rest-args-tys '()])
                         ([param param*])
                 (if (< i 6)
                     (values (+ i 1) rest-args-names rest-args-tys)
                     (values (+ i 1)
                             (cons (param-name param) rest-args-names)
                             (cons (param-type param) rest-args-tys))))])
           (let* ([last-arg-tup-name (gen-sym 'tup)]
                  [last-arg-ty (cons 'Vector (reverse rest-args-tys))]
                  [last-arg (list last-arg-tup-name ': last-arg-ty)])
             (let ([new-params (append (take param* 5) (list last-arg))]
                   [new-body ((limit-func-expr last-arg-tup-name
                                               (reverse rest-args-names))
                              body)])
               (Def name new-params rty info new-body)))))]))

(define (limit-functions p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map limit-func-def defs))]))

(define (func-type->vector-type type)
  (if (and (list? type) (memq '-> type))
      (list 'Vector (cons '(Vector _) type))
      type))

(define (convert-to-closures-lambda! params rty body)
  (define (new-lambda-def-body fvs fvs-param)
    (let loop ([fvs (set->list fvs)] [i 1])
      (if (null? fvs)
          (convert-to-closures-expr body)
          (Let (first fvs)
               ;  (Prim 'vector-ref (list fvs-param (Int i)))
               (Prim 'any-vector-ref
                     (list fvs-param (Int i))) ;;; dynamic typing
               (loop (rest fvs) (+ i 1))))))
  (let* ([fvs (set->list ((free-variables (list->set (map param-name params)))
                          body))]
         [fvs-param-name (gen-sym 'fvs)]
         [fvs-param (Var fvs-param-name)])
    (let ([a-new-lambda (new-lambda-def-body fvs fvs-param)]
          [a-new-lambda-name (gen-sym 'lambda)]
          [arity (length params)])
      (let ([res (Closure arity
                          (cons (FunRef a-new-lambda-name arity)
                                (map (lambda (sym) (Var sym)) fvs)))])
        (push!-lambda-defs
         (Def
          a-new-lambda-name
          (cons (list fvs-param-name
                      ':
                      (cons 'Vector
                            (cons '_
                                  (make-list (length fvs)
                                             'Integer)))) ; todo : other types
                params)
          (func-type->vector-type rty)
          '()
          a-new-lambda))
        res))))

(define (convert-to-closures-expr expr)
  ; (displayln expr)
  (match expr
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(ValueOf e ftype) (ValueOf e ftype)]
    [(Exit) (Exit)]
    [(FunRef label n) (Closure n (list (FunRef label n)))]
    [(Apply func args)
     (if (atm? func)
         ; (Prim 'any-vector-ref (list func (Int 0)))
         (Apply (Prim 'any-vector-ref (list func (Int 0))) ;;; dynamic typing
                (cons func (map convert-to-closures-expr args)))
         (let ([tmp (gen-sym 'tmp)])
           (Let tmp
                (convert-to-closures-expr func)
                ;  (Prim 'vector-ref (list (Var tmp) (Int 0)))
                (Apply (Prim 'any-vector-ref
                             (list (Var tmp) (Int 0))) ;;; dynamic typing
                       (cons (Var tmp) (map convert-to-closures-expr args))))))]
    [(Lambda params rty body) (convert-to-closures-lambda! params rty body)]
    [(Let var rhs body)
     (Let var (convert-to-closures-expr rhs) (convert-to-closures-expr body))]
    [(If cnd thn els)
     (If (convert-to-closures-expr cnd)
         (convert-to-closures-expr thn)
         (convert-to-closures-expr els))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (convert-to-closures-expr e)))]
    [(SetBang var rhs) (SetBang var (convert-to-closures-expr rhs))]
    [(Begin exps body)
     (Begin (map convert-to-closures-expr exps)
            (convert-to-closures-expr body))]
    [(WhileLoop cnd body)
     (WhileLoop (convert-to-closures-expr cnd) (convert-to-closures-expr body))]
    [(Collect n) (Collect n)]
    [(Allocate n ty) (Allocate n ty)]
    [(GlobalValue name) (GlobalValue name)]))

(define (convert-to-closures-def def)
  (match def
    [(Def name params rty info body)
     (let ([fvs (list (gen-sym 'fvs) ': '_)]) ;;; we don't care the type
       (Def name
            (if (eqv? name 'main) '() (cons fvs params))
            (func-type->vector-type rty)
            info
            (convert-to-closures-expr body)))]))

(define lambda-defs 'UINITIALIZED)

(define (init!-lambda-defs)
  (set! lambda-defs '()))

(define (push!-lambda-defs def)
  (set! lambda-defs (cons def lambda-defs)))

(define (convert-to-closures p)
  (init!-lambda-defs)
  (match p
    [(ProgramDefs info defs)
     (let ([new-defs (map convert-to-closures-def defs)])
       (ProgramDefs info (append lambda-defs new-defs)))]))

(define (make-func-type args-num)
  (append (make-list args-num 'Any) '(-> Any)))

(define (arithmetic-op? op)
  (or (eqv? op '+) (eqv? op '-)))

(define (bool-op? op)
  (or (eqv? op 'and) (eqv? op 'or) (eqv? op 'not)))

(define (cmp-op? op)
  (or (eqv? op 'eq?) (eqv? op '<) (eqv? op '<=) (eqv? op '>) (eqv? op '>=)))

(define (vector-op? op)
  (or (eqv? op 'vector)
      (eqv? op 'vector-ref)
      (eqv? op 'vector-length)
      (eqv? op 'vector-set!)))

(define (vector-type-length ty)
  (match ty
    [(cons 'Vector rest-ty) (length rest-ty)]
    [else (error 'vector-type-length)]))

(define (make-vector-type len)
  (cons 'Vector (make-list len 'Any)))

(define (func-type? type)
  (match type
    [`(,@rty -> ,_) #t]
    [else #f]))

(define (vector-type? ty)
  (match ty
    [`(Vector ,@t) #t]
    [else #f]))

(define (predicate-op? op)
  (memq op '(integer? boolean? vector? procedure? void?)))

(define (predicate-op->type op)
  (match op
    ['integer? 'Integer]
    ['boolean? 'Boolean]
    ['vector? '(Vector Any)]
    ['procedure? '(Any -> Any)]
    ['void? 'Void]))

(define (cast-insert-prim prim)
  (match prim
    [(Prim 'read (list)) (Inject (Prim 'read (list)) 'Integer)]
    [(Prim op args)
     #:when (arithmetic-op? op)
     (Inject (Prim op
                   (for/list ([arg args])
                     (Project (cast-insert-expr arg) 'Integer)))
             'Integer)]
    [(Prim op args)
     #:when (cmp-op? op)
     (Inject (Prim op
                   (for/list ([arg args])
                     (Project (cast-insert-expr arg) 'Integer)))
             'Boolean)]
    [(Prim op args)
     #:when (bool-op? op)
     (Inject (Prim op
                   (for/list ([arg args])
                     (Project (cast-insert-expr arg) 'Boolean)))
             'Boolean)]
    [(Prim 'vector-ref (list e1 e2))
     (let ([e1^ (cast-insert-expr e1)] [e2^ (cast-insert-expr e2)])
       (let ([vec-ty (Inject-type e1^)])
         (let ([vec-len (vector-type-length vec-ty)])
           (Prim 'any-vector-ref
                 (list (Project e1^ (make-vector-type vec-len))
                       (Project e2^ 'Integer))))))]
    [(Prim 'vector-length (list e1))
     (let ([e1^ (cast-insert-expr e1)])
       (let ([vec-ty (Inject-type e1^)])
         (let ([vec-len (vector-type-length vec-ty)])
           (Inject (Prim 'vector-length
                         (list (Project e1^ (make-vector-type vec-len))))
                   'Integer))))]
    [(Prim 'vector-set! (list e1 e2 e3))
     (let ([e1^ (cast-insert-expr e1)]
           [e2^ (cast-insert-expr e2)]
           [e3^ (cast-insert-expr e3)])
       (let ([vec-ty (Inject-type e1^)])
         (let ([vec-len (vector-type-length vec-ty)])
           (Prim 'any-vector-set!
                 (list (Project e1^ (make-vector-type vec-len))
                       (Project e2^ 'Integer)
                       e3^)))))]
    [(Prim 'vector args)
     (Inject (Prim 'vector
                   (for/list ([arg args])
                     (cast-insert-expr arg)))
             (make-vector-type (length args)))]

    [else (error 'cast-insert-prim)]))

(define (cast-insert-expr expr)
  (match expr
    [(Var x) (Var x)]
    [(Int n) (Inject (Int n) 'Integer)]
    [(Bool b) (Inject (Bool b) 'Boolean)]
    [(Void) (Void)]
    [(FunRef name arity) (FunRef name arity)]
    [(Let x rhs body) (Let x (cast-insert-expr rhs) (cast-insert-expr body))]
    [(If cnd thn els)
     (If (Prim 'eq? (list (cast-insert-expr cnd) (Inject (Bool #f) 'Boolean)))
         (cast-insert-expr els)
         (cast-insert-expr thn))]
    [(Prim op es) (cast-insert-prim expr)]
    [(Lambda params rty body)
     (let ([new-params (for/list ([param params])
                         (list param ': 'Any))]
           [new-rty 'Any]
           [new-body (cast-insert-expr body)])
       (Inject (Lambda new-params new-rty new-body)
               (make-func-type (length params))))]
    [(Apply f args)
     (let ([f^ (cast-insert-expr f)]
           [args^ (for/list ([arg args])
                    (cast-insert-expr arg))])
       (if (and (Inject? f^) (func-type? (Inject-type f^)))
           (Apply (Project f^ (make-func-type (length args))) args)
           (Apply f^ args)))]
    [(SetBang var rhs) (SetBang var (cast-insert-expr rhs))]
    [(Begin exps body)
     (Begin (for/list ([expr exps])
              (cast-insert-expr expr))
            (cast-insert-expr body))]
    [(WhileLoop cnd body)
     (WhileLoop (cast-insert-expr cnd) (cast-insert-expr body))]))

(define (cast-insert-def def)
  (match def
    [(Def name params rty info body)
     (let ([new-params (for/list ([param params])
                         (list param ': 'Any))]
           [new-rty 'Any])
       (Def name new-params new-rty info (cast-insert-expr body)))]))

(define (cast-insert p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info
                  (for/list ([def defs])
                    (cast-insert-def def)))]))

(define (tagof ftype)
  (match ftype
    ['Integer 1]
    ['Boolean 4]
    [`(Vector ,@tys) 2]
    [`(,@t -> ,rt) 3]
    ['Void 5]))

(define (vector-or-proc-tag? v)
  (or (eqv? v 2) (eqv? v 3)))

(define (reveal-casts-expr expr)
  (match expr
    [(Project e ftype)
     #:when (or (eqv? ftype 'Boolean) (eqv? ftype 'Integer))
     (let ([e^ (reveal-casts-expr e)] [tmp (gen-sym 'tmp)])
       (Let tmp
            e^
            (If (Prim 'eq?
                      (list (Prim 'tag-of-any (list (Var tmp)))
                            (Int (tagof ftype))))
                (ValueOf (Var tmp) ftype)
                (Exit))))]
    [(Project e ftype)
     #:when (func-type? ftype)
     (let ([e^ (reveal-casts-expr e)]
           [tmp (gen-sym 'tmp)]
           [expected-arity (- (length ftype) 2)])
       (Let tmp
            e^
            (If (Prim 'eq?
                      (list (Prim 'procedure-arity (list (Var tmp)))
                            (Int expected-arity)))
                (If (Prim 'eq?
                          (list (Prim 'tag-of-any (list (Var tmp)))
                                (Int (tagof ftype))))
                    (ValueOf (Var tmp) ftype)
                    (Exit))
                (Exit))))]
    [(Project e ftype)
     #:when (vector-type? ftype)
     (let ([e^ (reveal-casts-expr e)]
           [tmp (gen-sym 'tmp)]
           [expected-vec-len (- (length ftype) 1)])
       (Let tmp
            e^
            (If (Prim 'eq?
                      (list (Prim 'vector-length (list (Var tmp)))
                            (Int expected-vec-len)))
                (If (Prim 'eq?
                          (list (Prim 'tag-of-any (list (Var tmp)))
                                (Int (tagof ftype))))
                    (ValueOf (Var tmp) ftype)
                    (Exit))
                (Exit))))]
    [(Inject e ftype)
     (Prim 'make-any (list (reveal-casts-expr e) (Int (tagof ftype))))]
    [(Prim 'any-vector-ref (list e1 e2))
     (let ([e1^ (reveal-casts-expr e1)]
           [e2^ (reveal-casts-expr e2)]
           [tmp1 (gen-sym 'tmp)]
           [tmp2 (gen-sym 'tmp)])
       (Let tmp1
            e1^
            (Let tmp2
                 e2^
                 (If (Prim 'eq?
                           (list (Prim 'tag-of-any (list (Var tmp1)))
                                 (Int (tagof 'Vector))))
                     (If (Prim '<
                               (list (Var tmp2)
                                     (Prim 'any-vector-length
                                           (list (Var tmp1)))))
                         (Prim 'any-vector-ref (list (Var tmp1) (Var tmp2)))
                         (Exit))
                     (Exit)))))]
    [(Prim 'any-vector-set! (list e1 e2 e3))
     (let ([e1^ (reveal-casts-expr e1)]
           [e2^ (reveal-casts-expr e2)]
           [e3^ (reveal-casts-expr e3)]
           [tmp1 (gen-sym 'tmp)]
           [tmp2 (gen-sym 'tmp)])
       (Let
        tmp1
        e1^
        (Let tmp2
             e2^
             (If (Prim 'eq?
                       (list (Prim 'tag-of-any (list (Var tmp1)))
                             (Int (tagof 'Vector))))
                 (If (Prim '<
                           (list (Var tmp2)
                                 (Prim 'any-vector-length (list (Var tmp1)))))
                     (Prim 'any-vector-set! (list (Var tmp1) (Var tmp2) e3^))
                     (Exit))
                 (Exit)))))]
    [(Prim 'any-vector-length (list e1))
     (let ([e1^ (reveal-casts-expr e1)] [tmp1 (gen-sym 'tmp)])
       (Let tmp1
            e1^
            (If (Prim 'eq?
                      (list (Prim 'tag-of-any (list (Var tmp1)))
                            (Int (tagof 'Vector))))
                (Prim 'any-vector-length (list (Var tmp1)))
                (Exit))))]
    [(Var x) (Var x)]
    [(Prim op (list e))
     #:when (predicate-op? op)
     (let ([e^ (reveal-casts-expr e)] [tmp (gen-sym 'tmp)])
       (If (Prim 'eq?
                 (list (Prim 'tag-of-any (list (Var tmp)))
                       (tagof (predicate-op->type op))))
           (Bool #t)
           (Bool #f)))]

    [(Var x) (Var x)]
    [(Void) (Void)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(FunRef name arity) (FunRef name arity)]
    [(Let x rhs body) (Let x (reveal-casts-expr rhs) (reveal-casts-expr body))]
    [(If cnd thn els)
     (If (reveal-casts-expr cnd)
         (reveal-casts-expr thn)
         (reveal-casts-expr els))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (reveal-casts-expr e)))]
    [(Lambda params rty body) (Lambda params rty (reveal-casts-expr body))]
    [(Apply f args)
     (Apply (reveal-casts-expr f)
            (for/list ([arg args])
              (reveal-casts-expr arg)))]
    [(SetBang var rhs) (SetBang var (reveal-casts-expr rhs))]
    [(Begin exps body)
     (Begin (for/list ([expr exps])
              (reveal-casts-expr expr))
            (reveal-casts-expr body))]
    [(WhileLoop cnd body)
     (WhileLoop (reveal-casts-expr cnd) (reveal-casts-expr body))]))

(define (reveal-casts-def def)
  (match def
    [(Def name params rty info body)
     (Def name params rty info (reveal-casts-expr body))]))

(define (reveal-casts p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info
                  (for/list ([def defs])
                    (reveal-casts-def def)))]))
