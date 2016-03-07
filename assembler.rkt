#lang racket/base

(require (for-syntax racket/base racket/syntax))

;; We want to generate a fast translation
;; from `(op args ...) to (bytes b0  ... bn)

;; We would like to specify this syntax like so
#;(define-instruction-table x86-64-instruction-table)
#;(add-intruction x86-64-instruction-table
   [(ADD AL   imm8)  (x86-64-opcode 04 ib)]
   [(ADD r64  r/m64) (x86-64-opcode REX.W + 03 /r)])
#;(add-instruction x86-64-instruction-table
   [(MOV moffs64 RAX)    ...]
   [(MOV r64 imm64)  ...]
   [(MOV r64     r/m64)  (x86-64-opcode REX.W + 89 /r)])

;; one can immagine that the a single clause works
;;  [(MOV moffs64,RAX) ;;arbitrary-code to handle special cases]
;;  [(MOV r64, imm64)  ;; appropriate code here]
;;  [(MOV r64, r/m64)  (x86-64-opcode REX.W + 89 /r)])
#;(define (encode-MOV a1 a2)
    (cond
      [(reg64? a1)
       (let ([reg a1])
         (cond
           [(imm64? a2) #|code for opcode|#]
           [(reg64? a2) #|code for opcode|#]
           [else (error 'encode-mov "undefined for arg 2 = ~a")]))]
       [(moffs64? a1) #|code for arg 2 subcases |# ]
       [else (error 'encode-mov "(MOV ~a ~a)" a1 a2)]))

;; One lingering problem is how to efficiently transfer information
;; from the argument types to the opcodes in order to efficiently
;; encode information

;; Instruction-Map Symbol (U Index #f) Proc? -> (Void)
(define (add-instruction-encoder! table name arity encoder)
  (unless (and (hash? table)
               (symbol? name)
               (fixnum? arity)
               (procedure? encoder))
    (error 'add-instruction-encoder!))
  (hash-set! table name (cons arity encoder)))

(define (encode-instruction itable instr)
  (when (null? instr)
    (error 'encode-instruction "given null"))
  (define memn (car instr))
  (define a.e? (hash-ref itable memn #f))
  (unless (pair? a.e?)
    (error 'encode-instruction "memnomic not found ~a" instr))
  (define arity (car a.e?))
  (define encoder (cdr a.e?))
  (unless (procedure? encoder)
    (error 'encode-instruction "This shouldn't happen"))
  (cond
    [(eq? arity 0) (encoder)]
    [(eq? arity 1) (static-apply encoder 1 (cdr instr))]
    [(eq? arity 2) (static-apply encoder 2 (cdr instr))]
    [(eq? arity 3) (static-apply encoder 3 (cdr instr))]
    [(not arity)   (apply encoder (cdr instr))]
    [else (error 'encode-instruction "bad arity ~a" arity)]))


(define-syntax (static-apply s)
  (syntax-case s ()
    [(_ p n a*) #'(apply p a*)]))

(define-syntax (add-instruction stx)
  (syntax-case stx ()
    [(_ (name table x a) [(n p ...) e** ...] ...)
     (with-syntax ([encode-name
                    (format-id #'name "encode-~a" (syntax-e #'name))])
       #'(begin
           (define (encode-name x a)
             (match x
               [`(,_ p ...) e** ...] ...
               [otherwise (error 'encode-name "~a" x)]))
           (add-instruction-encoder! table 'name (cons #f encode-name))))]))


