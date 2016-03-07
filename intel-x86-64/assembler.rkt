#lang racket/base

(require
  racket/match
  racket/fixnum
  racket/stxparam
  "../assembler.rkt"
  "../asm.rkt"
  "registers.rkt"
  "../helpers.rkt")
  
;(provide x86-64-instruction-table)

(module+ test (require rackunit))



;; Machine Constants

(define LEAST-S32-INTEGER    (- (expt 2 31)))
(define GREATEST-S32-INTEGER (- (expt 2 31) 1))

(define LEAST-S64-INTEGER    (- (expt 2 63)))
(define GREATEST-S64-INTEGER (- (expt 2 63) 1))


;; ASM-Lang helpers and discription





(define mem? disp?)

(define (small-disp? x)
  (and (disp? x) (byte? (disp-offset x))))

;; The Mod R/M byte ie the addressing form specifier byte
;; mod field combines with the r/m field to form 32 posible values
;; -- 8 registers and 24 addressing modes
;; reg/op field specifies either a register number or three
;; -- more bits of opcode informations
;; r/m field can specidfy a register as an operand or be combined
;; -- with the mode field to encode an addressing mode.
#;(define (ModR/M eaddr reg/opd #:eaddr+disp [disp #f])
  ;; Calculate the 3 bit r/m field
  (define r/m (or (reg? eaddr)
                  (sib? eaddr)(and (eq? 'SIB eaddr) #b100)
                  (and (eq? 'disp32 eaddr) #b101)
                  (and (register-info? eaddr)
                       (register-info-index eaddr))
                  (error 'ModR/M "invalid effective address ~a" eaddr)))
  ;; Calculate the 2 bit mod field
  (define mod (if (register-info? eaddr)
                  (cond
                    [(not disp) (register-info-mod eaddr)]
                    [(eq? 8 disp) #b01]
                    [(eq? 32 disp) #b10]
                    [else (error 'ModR/M "invalid displacement ~a" disp)])
                  ;; We know that eaddr must be 'SIB or 'disp32 because
                  ;; otherwise the code would have aborted above
                  #b00))
  (define reg (cond
                [(register-info? reg/opd) (register-info-index reg/opd)]
                [(opcode-info? reg/opd)   (opcode-info-reg reg/opd)]))
  (byte
   (fxior (fxlshift mod 6)
          (fxior (fxlshift reg 3)
                 r/m))))

;; The SIB byte describes addressing information in conjuction
;; with the mod byte.
;; scale is
#;(define (SIB scale index-reg base-reg)
  (define ss
    (cond
      [(eq? 1 scale) #b00]
      [(eq? 2 scale) #b01]
      [(eq? 4 scale) #b10]
      [(eq? 8 scale) #b11]
      [else (error 'SIB "invalid scale ~a" scale)]))
  (define index
    ;; There are some very strange cases
    (if (register-info? index-reg)
        (register-info index-reg)
        (error 'SIB "invalid index register ~a" index-reg)))
  (define base
    (if (register-info? base-reg)
        (register-info  base-reg)
        (error 'SIB "invalid base register ~a" base-reg)))
  (byte (fxior (fxlshift ss 6)
               (fxior (fxlshift index 3)
                      base))))

#;(define (REX w #:ModR/M-reg [reg #f]
               #:SIB-index  [idx #f]
               #:rm/base/op [oth #f])
  (define W 1)
  (define R (or (and (register-info? reg) (register-info-rex reg))
                (and (not reg) 0)
                (error 'REX "invalid base register ~a" reg)))
  (define X (or (and (register-info? idx) (register-info-rex idx))
                (and (not idx) 0)
                (error 'REX "invalid base register ~a" idx)))
  (define B (or (and (register-info? oth) (register-info-rex oth))
                (and (not oth) 0)
                (error 'REX "invalid base register ~a" oth)))
  (byte
   (fxior
    (fxlshift #b0100 4)
    (fxior
     (fxlshift W 3)
     (fxior
      (fxlshift R 2)
      (fxior (fxlshift X 1) B))))))
   
         
(define (int32? x)
  (and (integer? x)
       (<= LEAST-S32-INTEGER x)
       (<= x GREATEST-S32-INTEGER)))

(define (int64? x)
  (and (integer? x)
       (<= LEAST-S64-INTEGER x)
       (<= x GREATEST-S64-INTEGER)))

         

(define (IMM n ac)
  (cond
    [(int32? n)
     (cons* (byte n)
            (byte (sra n 8))
            (byte (sra n 16))
            (byte (sra n 24))
            ac)]
    [(int64? n)
     (cons* (byte n)
            (byte (sra n 8))
            (byte (sra n 16))
            (byte (sra n 24))
            (byte (sra n 32))
            (byte (sra n 40))
            (byte (sra n 48))
            (byte (sra n 56))
            ac)]
    #;[(label-address? n) (cons (cons 'label-addr (label-name n)) ac)]
    #;[(foreign? n) (cons (cons 'foreign-label (label-name n)) ac)]
    #;[(label? n)
     (cond
       [(local-label? (label-name n))
        (cons (cons 'local-relative (label-name n)) ac)]
       [else (cons (cons 'relative (label-name n)) ac)])]
    [else (error 'IMM "invalid input ~a" n)]))


(define (IMM8 n ac)
  (cond
    [(int? n) (cons (byte n) ac)]
    [else (error 'IMM* "invalid ~a" n)]))

(define (imm? x)
  (or (int? x)
      (obj? x)
      (obj+? x)
      (label-address? x)
      (foreign? x)
      (label? x)))

(define (foreign? x)
  (and (pair? x) (eq? (car x) 'foreign-label)))

(define (imm8? x)
  (and (int? x) (byte? x)))

(define (label? x)
  (and (pair? x) (eq? (car x) 'label)))

(define (label-address? x)
  (and (pair? x) (eq? (car x) 'label-address)))

(define (label-name x) (cadr x))

(define (int? x) (integer? x))

(define (obj? x) (and (pair? x) (eq? (car x) 'obj)))

(define (obj+? x) (and (pair? x) (eq? (car x) 'obj+)))

#;(define (CODErri c d s i ac)
  (cond
    [(imm8? x) (CODE c (ModRM 1 d s (IMM8 i ac)))]
    [else      (CODE c (ModRM 2 d s (IMM  i ac)))]))

#;(define (CODErr c r1 r2 ac)
  (CODE c (ModRM 3 r1 r2 ac)))

#;(define (RegReg r1 r2 r3 ac)
  (cond
    [(eq? r3 'rsp) (error 'RegReg "Bug: invalid src rsp")]
    [(eq? r1 'rbp) (error 'RegReg "Bug: invalid src rbp")]
    [else (cons* (fxior 4 (fxlshift (reg-index r1) 3))
                 (fxior (register-index r2) (fxlshift (reg-index r3) 3))
                 ac)]))

#;(define (IMM*2 i1 i2 ac)
  (cond
    [(and (int? i1) (obj i2))
     (let ([d i1] [v (cadr i2)])
       (cons (reloc-word+ v d) ac))]
    [(and (int? i2) (obj i1))
     (let ([d i2] [v (cadr i1)])
       (cons (reloc-word+ v d) ac))]
    [(and (int? i1) (int? i2))
     (IMM (bitwise-and (+ i1 i2)
                       (- (expt 2 (* wordsize 8)) 1))
          ac)]
    [else (error 'IMM*2 "Invalid ~a ~a" i1 i2)]))

#;(define (imm32? x)
  (case wordsize
    [(4) (imm? x)]
    [(8) (and (integer? x)
              (<= (- (expt 2 31)) x)
              (<= x (- (expt 2 31) 1)))]
    [else (error 'imm32 "wordsize invalid ~a" wordsize)]))

(define intruction-map (make-hasheq))

(define (convert-instruction a ac)
  (cond
    [(hash-ref intruction-map (car a) #f)
     =>
     (lambda (p)
       (let ([n (car p)] [proc (cdr p)] [args (cdr a)])
         (cond
           [(fx= n 2)
            (if (fx= (length args 2))
                (proc a ac (car args) (cadr args))
                (error 'conver-intruction "argument mismatch ~a" a))]
           [(fx= n 1)
            (if (fx= (length args) 1)
                (proc a ac (car args))
                (error 'conver-intruction "argument mismatch ~a" a))]
           [(fx= n 0)
            (if (fx= (length args) 0)
                (proc a ac)
                (error 'conver-intruction "argument mismatch ~a" a))]
           [else
            (if (fx= (length args) n)
                (apply proc a ac args)
                (error 'conver-intruction "argument mismatch ~a" a))])))]
    [(eq? (car a) 'seq) (foldr convert-instruction ac (cdr a))]
    #;[(eq? (car a) 'pad)
     (define (find-prefix x ls)
       (let f ([ls ls])
         (if (eq? ls x)
             '()
             (let ([a (car ls)])
               (if (and (pair? a) (eq? (car a) 'bottom-code))
                   (f (cdr ls))
                   (cons a (f (cdr ls))))))))
     (let ([n (cadr a)] [code (cddr a)])
       (let ([ls (foldr convert-instruction ac code)])
         (let ([m (compute-code-size (find-prefix ac ls))])
           (append (make-list (- n m) 0) ls))))]
    [else (error 'convert-instruction "unknown instuction" a)]))

(require (for-syntax racket/base racket/syntax racket/list))

(define instruction-map (make-hasheq))

(define-syntax (define-instruction stx)
  (syntax-case stx ()
    [(_ (name x ...) [(p ...) e** ...] ...)
     (with-syntax ([encode-name
                    (format-id #'name "encode-~a" (syntax-e #'name))])
       #'(begin
           (define (encode-name sexp)
             (apply (lambda (x ...)
                      (operand-case 'name (x ...)
                        [(p ...) e** ...] ...))
                    (cdr sexp)))
           (hash-set! instruction-map 'name encode-name)))]))

(define-syntax (add-instruction stx)
  (define (get-arg-count r**)
    (cond
      [(not (pair? r**))
       (error 'add-instructions/get-arg-count "precodition violated")]
      [(pair? (cdr r**))
       (let ([l (length (syntax->list (car r**)))])
         (if (= l (get-arg-count (cdr r**)))
             l
             (raise-syntax-error 
              'add-instruction "uneven argument lists" stx)))]
      [else (length (syntax->list (car r**)))]))
  (syntax-case stx ()
   [(_ name table)
    (raise-syntax-error 'add-instruction 
      "no clauses"  stx)]
   [(_ name table [(r** ...) c** ...] ...)
    (let ([n (get-arg-count (syntax->list #'((r** ...) ...)))])
      (with-syntax ([(x ...) (generate-temporaries (make-list n #'tmp))])
        #`(hash-set! 
           table
           'name
           (cons #,n
                 (lambda (x ...)
                   (operand-case 'name (x ...)
                    [(r** ...) c** ...] ...))))))]))

;; Add an opcode byte to the head of the byte list
(define (CODE opcode acc)
  (cons (byte! opcode) acc))

;; Add an opcode that expects a register to the head of the byte list
(define (CODE+reg opcode reg acc)
  (cons (byte! (fxior opcode (register-index reg))) acc))

(define (C opcode acc)
  (REX.R 0 (CODE opcode acc)))

(define (CR opcode reg acc)
  (REX-reg reg (CODE+reg opcode reg acc)))

(define (CR* opcode reg r/m acc)
  (REX-r/m reg r/m (CODE opcode (R/M reg r/m acc))))

;; Compute the Mod and Immediate Bytes needed
(define (R/M /d dst acc)
  (unless (reg? /d)
    (error 'R/M "/d field must be a register: ~a" /d))
  (if (reg? dst)
      (MOD-reg #b11 /d dst acc)
      (error 'R/M "todo")))

;; Computer the ModR/M byte for type registers
(define (MOD-reg mod reg r/m acc)
  (unless (and (byte? mod) (reg? reg) (reg? r/m))
    (error 'MOD-R/M-reg-reg "invalid arguments ~a" (list mod reg r/m)))
  (MOD-R/M mod (register-index reg) (register-index r/m) acc))


(define (MOD-R/M mod reg r/m acc)
  (unless (and (byte? mod) (<= 0 mod 3)
               (byte? reg) (<= 0 reg 7)
               (byte? r/m) (<= 0 r/m 7))
    (error 'MOD-R/M "invalid input ~a" (list mod reg r/m)))
  (cons (fxior (fxlshift mod 6) (fxior (fxlshift reg 3) r/m)) acc))

(define-syntax-rule (REX W R X B acc)
  (begin
    (unless (and (boolean? W) ;; Are the operands 64bit size
                 (boolean? R) ;; is the MOD-R/M reg field extended
                 (boolean? X) ;; is the SIB index field extended
                 (boolean? B));; is the r/m, base, or opcode extended
      (error 'REX "invalid input ~a" (list W R X B)))
    (let* ([b (byte! #b01000000)]
           [b (if W (fxior b #b1000) b)]
           [b (if R (fxior b #b0100) b)]
           [b (if X (fxior b #b0010) b)]
           [b (if B (fxior b #b0001) b)])
      (if (eq? b #b01000000)
          acc
          (cons b acc)))))

(define (REX.R reg bits acc)
  (unless (and (byte? bits) (reg? reg))
    (error 'REX.R "invalid input ~a" bits))
  (let ([b (fxior (if (reg64? reg) #b01001000 #b01000000) bits)])
    (if (eq? bits #b01000000)
        acc
        (cons b acc))))

(define (REX.W acc)
  (REX #t #f #f #f acc))

;; Include the REX byte deciding if the setting the B bit
(define (REX-reg reg acc)
  (unless (reg? reg)
    (error 'REX+reg "invalid input ~a" reg))
  (if (register-rex-bit? reg)
      (REX.R reg #b001 acc)
      (REX.R reg #b000 acc)))

(define (REX-r/m reg r/m acc)
  (unless (and (reg? reg) (or (reg? r/m) (mem? r/m)))
    (error 'REX+r/m "invalid input ~a ~a" reg r/m))
  (REX (reg64? reg)
       (register-rex-bit? reg)
       #f
       (and (reg? r/m) (register-rex-bit? r/m))
       acc))

(define (REX-r2 reg r/m acc)
  (REX (reg64? reg)
       (register-rex-bit? reg)
       #f
       (register-rex-bit? r/m)
       acc))

(define (code-reg-reg opcode r/m reg acc)
  (REX-r2 reg r/m (CODE opcode (MOD-reg #b11 reg r/m acc))))

;; Instruction Encoding Definitions

(define-instruction (mov src dst)
  ;;In 64-bit mode, r/m8 can not be encoded to access
  ;;the following byte registers if a REX prefix is used: AH, BH, CH, DH
  [(reg8? reg8?)
   (if (and (or (eq? dst %ah)(eq? dst %bh)
                (eq? dst %ch)(eq? dst %dh))
            (register-rex-bit? src))
       ;; Bypass the issue by encoding ah, bh, ch, dh in reg
       (code-reg-reg #x8A src dst '())
       (code-reg-reg #x88 dst src '()))]
  [(reg? reg?)
   (unless (= (register-size src) (register-size dst))
     (error 'encode-mov "incompatable register sizes ~a ~a" src dst))
   (code-reg-reg #x89 dst src '())]
  ;; movb
  [(reg8? disp?)
   (error 'encode-mov "TODO: movb reg, mem")]
  [(imm8? disp?)
   (error 'encode-mov "TODO: movb imm, mem")]
  [(disp? reg8?)
   (error 'encode-mov "TODO: movb mem, reg")]
  ;; movl
  #;[(mov ,(? imm32? src) ,(? reg? dst))
   (error 'encode-mov "TODO: movb reg, mem")]
  #;[(mov ,(? imm32? src) ,(? disp? dst))
   (error 'encode-mov "TODO: movb imm, mem")]
  #;[(mov ,(? disp? src)  ,(? reg32? dst))
     (error 'encode-mov "TODO: movb mem, reg")])

(module+ test
  (check-equal? (encode-mov `(mov ,%ch ,%ah))
                '(#x88 #xec))
  (check-equal? (encode-mov `(mov ,%cl ,%al))
                '(#x88 #xc8))
  #;(check-equal? (encode-mov `(mov ,%cx  ,%ax))
                ;; currently where does the #66 come from
                '(#x66 #x88 #xec))
  (check-equal? (encode-mov `(mov ,%r12d  ,%eax))
                '(#x44 #x89 #xe0))
  (check-equal? (encode-mov `(mov ,%rdx ,%rax))
                '(#x48 #x89 #xd0))
  (check-equal? (encode-mov `(mov ,%esp ,%ebp))
                '(#x89 #xe5))
  )


(define-instruction (ret)
  [() (CODE #xc3 '())])

(require (for-syntax racket/list))

(define-syntax (add-instructions stx)
  ;; Group all of the clauses that are defined for the same instruction
  ;; together
  ;;([(op c ...) c ...] ...) -> ([op [(c ...) c ...] ...] ...)
  (define (seperate-instructions ls)
    (define (reformat x)
      (with-syntax ([[(Op . r) . c] x])
        (cons #'Op #'(r . c))))
    (define (help a a.d*)
      (partition (lambda (a.d) (free-identifier=? a (car a.d))) a.d*))
    (let loop ([res '()][a-ls (map reformat ls)])
      (cond
        [(null? a-ls) res]
        [else
         (let*-values ([(a d) (values (car a-ls) (cdr a-ls))]
                       [(op clause) (values (car a) (cdr a))]
                       [(same other) (help op d)]
                       [(same) (map cdr same)])
           (loop (cons (cons op (cons clause same)) res)
                 other))])))
  (syntax-case stx (public)
    [(_ table [(Op* rand** ...) code** ... opcode*] ...)
     (with-syntax ([([Op* [(rand?*** ...) code*** ... opcode**] ...] ...)
                    (seperate-instructions 
                     (syntax->list 
                      #'([(Op* rand** ...) code** ... opcode*] ...)))])
       #'(let ([tmp table])
           (add-instruction Op* tmp
             [(rand?*** ...) code*** ... opcode**] ...)
           ...))]))

(module+ test
  (check-equal? (encode-ret '(ret)) '(#xc3)))

(define-syntax-rule (define-instruction-encoder (name table))
  (define (name instruction)
    (unless (and (hash? table)
                 (list? instruction)
                 (>= (length instruction) 1))
      (error 'name "given bad arguments ~a ~a" table instruction))
    (let* ([op (car instruction)]
           [args (cdr instruction)]
           [n  (length (cdr instruction))]
           [a.d    (hash-ref table op #f)]
           [arity  (if a.d (car a.d) #f)]
           [encode (if a.d (cdr a.d) #f)])
      (cond
        [(not a.d)    (error 'instruction-encoder "instruction not found")]
        [(eq? args 0) (encode)]
        [(eq? args 1) (encode (car args))]
        [(eq? args 2) (encode (car args) (cadr args))]
        [(eq? args 3)
         (let ([d (cdr args)])
           (encode (car args) (car d) (cadr d)))]
        [else (apply encode args)]))))

(define-instruction-encoder (encode-x64 instruction-map))

;;In 64-bit mode, r/m8 can not be encoded to access
  ;;the following byte registers if a REX prefix is used: AH, BH, CH, DH
(add-instructions instruction-map
 [(MOV reg8? reg8?)
  (if (and (or (eq? dst %ah)(eq? dst %bh)
               (eq? dst %ch)(eq? dst %dh))
           (register-rex-bit? src))
      ;; Bypass the issue by encoding ah, bh, ch, dh in reg
      (code-reg-reg #x8A src dst '())
      (code-reg-reg #x88 dst src '()))]
 [(MOV reg? reg?)
  (unless (= (register-size src) (register-size dst))
    (error 'encode-mov "incompatable register sizes ~a ~a" src dst))
  (code-reg-reg #x89 dst src '())]
 [(RET) (CODE #xc3 '())]
 [(ADD reg8?) (error 'todo "encode ADD")])








