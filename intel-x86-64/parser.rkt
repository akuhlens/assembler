#lang racket
#|
"88 /r"         "MOV r/m8,r8"       "MR" "Valid" "Valid" "Move r8 to r/m8."
"REX + 88 /r"   "MOV r/m8***,r8***" "MR" "Valid" "N.E."  "Move r8 to r/m8."
"89 /r"         "MOV r/m16,r16"     "MR" "Valid" "Valid" "Move r16 to r/m16."
"89 /r"         "MOV r/m32,r32"     "MR" "Valid" "Valid" "Move r32 to r/m32."
"REX.W + 89 /r" "MOV r/m64,r64"     "MR" "Valid" "N.E."  "Move r64 to r/m64."
"8A /r"         "MOV r8,r/m8"       "RM" "Valid" "Valid" "Move r/m8 to r8."
"REX + 8A /r"   "MOV r8***,r/m8***" "RM" "Valid" "N.E."  "Move r/m8 to r8."
=<parse>
(lambda (instr)
  (instruction-case instr
   [(MOV (r/m 8)  (r 8))   (88 /r)]
   [(MOV (r/m 8)  (r 8))   ...]
   [(MOV (r/m 16) (r 16))  (89 /r)]
   [(MOV (r/m 32) (r 32))  (89 /r)]
   [(MOV (r/m 64) (r 64))  (REX.W + 89 /r)]
   [(MOV (r 8)    (r/m 8)) (8A /r)]
   [(MOV (r 8)    (r/m 8)) ...]))
=<compile/match>
(lambda (instr)
 ;; use explicit list structure so that match macro generates better code
 [`(MOV (mem ,(or #f 8) ,r/m) (reg 8 ,reg))
   (syntax-parameterize ([instruction-context `((r/m 8 ,r/m) (r 8 ,reg))])
    (opcode 88 /r)
|#

(require (for-syntax syntax/parse "syntax-classes.rkt"))
(require racket/stxparam racket/stxparam-exptime)
(require "registers.rkt" "memory.rkt")

(define-syntax-parameter r/m-variable #f)
(define-syntax-parameter r/m-size     #f)
(define-syntax-parameter reg-variable #f)
(define-syntax-parameter reg-size     #f)
(begin-for-syntax
  
  (define-syntax-class argument-pattern
    #:datum-literals (r/m r)
    (pattern ((~and r/m bind:id) n:valid-gpr-size-class)
             #:with pattern #'(,(or 'memory 'register) 64 ,bind)                                  
             #:with context #`([r/m-variable #'bind]
                               [r/m-size    '#,(attribute n.datum)]))
    (pattern ((~and r   bind:id) n:valid-gpr-size-class)
             #:with pattern #'(register n ,bind)
             #:with context #`([reg-variable #'bind]
                               [reg-size    '#,(attribute n.datum)])))
  
  (define-syntax-class instruction-pattern
    (pattern (instr:id arg*:argument-pattern ...)
             #:with pattern #'`(instr arg*.pattern ...)
             #:do   [(define ctx
                       (with-syntax ([((p ...) ...) #'(arg*.context ...)])
                         #'(p ... ...)))]
             #:with context ctx)))

(define-syntax (build-instruction-case stx)
  (syntax-parse stx
    [(_ [i*:instruction-pattern e**:expr ...] ...)
     #'(lambda (instr)
         (match instr
           [i*.pattern
            (syntax-parameterize i*.context
                                 e** ...)]
           ...
           [otherwise (error 'match-instruction "unmatched ~a" otherwise)]))]))

(define-for-syntax (partition-cases instr* case*)
  (hash-values
   (for/fold ([h (hasheq)])
             ([i (in-list (syntax->list instr*))]
              [c (in-list (syntax->list case*))])
     (let ([s (syntax->datum i)])
       (unless (symbol? s)
         (error 'partition-cases "expected symbol: ~a" s))
       (with-syntax ([(instr cases ...) (hash-ref h s #`(#,i))])
         (hash-set h s #`(instr cases ... #,c)))))))
  
(define-syntax (define-instruction-encoder stx)
  (syntax-parse stx
    ;; Mockup definition
    [(_ encoder:id
        (~and [(instr*:id . args-rest) . rhs-rest] case-stx*) ...)
     (with-syntax ([table-id #'table]
                   [((unique-instr* instr-case** ...) ...)
                    (partition-cases #'(instr* ...) #'(case-stx* ...))])
       #'(begin
           (define table-id (make-hasheq))
           (hash-set! table-id
                      'unique-instr*
                      (build-instruction-case instr-case** ...))
           ...
           (define (encoder instr)
             (match instr
               [(cons memnomic rest)
                (let ([encoder? (hash-ref table-id memnomic #f)])
                  (if encoder?
                      (encoder? instr)
                      (error 'encoder "invalid memnomic: ~a" memnomic)))]
               [else (error 'encoder "invalid instruction: ~a" instr)]))))]))



(require (for-syntax racket))

(begin-for-syntax

  (define-splicing-syntax-class instruction-prefix-class
    (pattern (~seq (~datum REX.W) (~datum +))
             #:with compute #'(list (compute-rex #:W #t)))
    (pattern (~seq)
             #:with compute #'(list (compute-rex))))
  
  (define-splicing-syntax-class opcode-class
    (pattern (~seq any)
             #:do [(define datum (syntax-e #'any))
                   (define num?
                     (cond
                       [(number? datum)
                        (string->number (number->string datum) 16)]
                       [(symbol? datum)
                        (string->number (symbol->string datum) 16)]
                       [else #f]))]
             #:when num?
             #:with compute #`'(#,num?)))
  
  (define-splicing-syntax-class ModR/M-class
    (pattern (~seq (~datum /r))
             #:do [(define r/m (syntax-parameter-value #'r/m-variable))
                   (define reg (syntax-parameter-value #'reg-variable))]
             #:with compute #`(list (compute-ModR/M #,r/m #,reg)))
    (pattern (~seq)
             #:with compute #''())))

(define (compute-rex #:W [w #f] #:R [r #f] #:X [x #f] #:B [b #f])
  (let* ([n #b01000000]
         [n (if w (bitwise-ior n #b1000) n)]
         [n (if r (bitwise-ior n #b100)  n)]
         [n (if x (bitwise-ior n #b10)   n)]
         [n (if b (bitwise-ior n #b1)    n)])                  
    n))

(define (reg->oct x)
  (let ([r? (name->register x)])
    (unless r?
      (error 'reg->oct "invalid register name ~a" x))
    (register-octal r?)))


(define (compute-ModR/M r/m reg)
  (define MOD #b11000000)
  (define REG
    (arithmetic-shift
     (cond
      [(and (fixnum? reg) (<= 0 reg 7)) reg]
      [else (reg->oct reg)])
     3))
  (define R/M
    (cond
      [(register-name? r/m) (reg->oct r/m)]
      [(memory? r/m)   (error 'compute-mod-rm "todo memory")]
      [else (error 'compute-ModR/M "invalid ~a" r/m)]))
  (bitwise-ior MOD REG R/M))



(define-syntax (opcode stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ pre:instruction-prefix-class op:opcode-class mod:ModR/M-class)
     #'(let ()
         (append pre.compute op.compute mod.compute))]))

(define test1 '(MOV (register 64 RAX) (register 64 RCX)))
(define test2 '(MOV (register 64 R8)  (register 64 R10)))
(define test3 '(MOV (register 64 RCX) (register 64 RAX)))
(define test4 '(FOO (register 64 RBX) (register 64 RBP)))


(define-instruction-encoder encode-x86-64
  [(MOV (r/m 64) (r 64)) (opcode REX.W + 89 /r)]
  [(MOV (r/m 32) (r 32)) (opcode 89 /r)]
  [(MOV (r/m 16) (r 16)) (opcode 89 /r)]
  [(MOV (r/m 64) (r 64)) (opcode REX.W + 89 /r)]
  [(ADD (r/m 64) (r 64)) (opcode REX.W + 89 /r)])

