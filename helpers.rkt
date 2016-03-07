#lang racket/base

(require racket/fixnum
         (for-syntax racket/base racket/list))
(provide
 cons*
 byte
 byte!
 condition)

;; Contracts


;; Helpers that should be moved out later

(define-syntax cons*
  (syntax-rules ()
    [(_ a d) (cons a d)]
    [(_ a d c ...) (cons a (cons* d c ...))]))

;; Mask off a byte of a number
(define-syntax-rule (byte x)
  (let ([t x])
    (if (exact-integer? t)
        (bitwise-and t 255)
        (error 'byte "invalid argument ~a" x))))

(define-syntax-rule (byte! x)
  (let ([t x])
    (unless (byte? t)
      (error 'byte! "assertion that ~a was a byte failed" t))
    t))

(define dh-map
  (make-immutable-hash
   '((0 . "0")(1 . "1")(2 . "2")(3 . "3")(4 . "4")
    (5 . "5")(6 . "6")(7 . "7")(8 . "8")(9 . "9")
    (10 . "a")(11 . "b")(12 . "c")(13 . "d")
    (14 . "e")(15 . "f"))))

(define hd-map
  (make-immutable-hash
   (for/list ([(k v) (in-hash dh-map)]) (cons v k))))

(provide digit->hex hex->digit byte->hex)
(define (digit->hex x)
  (hash-ref dh-map x (lambda () (error 'digit->hex))))

(define (hex->digit x)
  (hash-ref hd-map x (lambda () (error 'hex->digit))))

(define (byte->hex x)
  (format "#x~a~a"
          (digit->hex (quotient x 16))
          (digit->hex (remainder x 16))))


(provide byte->hex*)
(define (byte->hex* x) (map byte->hex x))


(define (hex->byte x)
  (unless (and (string? x)
               (= (string-length x) 4)
               (eq? (string-ref x 0) #\#)
               (eq? (string-ref x 1) #\x))
    (error 'hex->byte "~a" x))
  (string->number x))

(provide hex->byte hex->byte*)
(define (hex->byte* x) (map hex->byte x))

(provide byte->bin)
(define (byte->bin x)
  (format "#b~a~a~a~a~a~a~a~a"
          (fxand 1 (fxrshift x 7))
          (fxand 1 (fxrshift x 6))
          (fxand 1 (fxrshift x 5))
          (fxand 1 (fxrshift x 4))
          (fxand 1 (fxrshift x 3))
          (fxand 1 (fxrshift x 2))
          (fxand 1 (fxrshift x 1))
          (fxand 1 (fxrshift x 0))))


;; Shift right arithmetic
(provide sra)
(define (sra n m)
  (arithmetic-shift n (- m)))


;; casing on identifiers efficiently
(provide operand-case)
(define-syntax (operand-case stx)
  (syntax-case stx ()
    [(_ who (operand* ...) [(operand?** ...) code0* code** ...] ...)
     (let* ([rand*   (syntax->list #'(operand* ...))]
            [len     (length rand*)]
            [len=?   (lambda (x) (= (length x) len))]
            [rand?** (map syntax->list 
                          (syntax->list #'((operand?** ...) ...)))])
       (cond
         [(not (andmap len=? rand?**)) 
          (raise-syntax-error 'operand-case "arity mismatch" stx)]
         [(not (andmap identifier? rand*))
          (raise-syntax-error 'operand-case "operands must be identifiers" stx)]
         [else
          (syntax/loc stx (operand-case-aux who () () (operand* ...)
           [(operand?** ...) (let () code0* code** ...)] ...))]))]
    [(_ who (operand* ...) 
      [(?* ...) c0 c1 ...] ...
      [(? ...)] . rest)
     (raise-syntax-error 'operand-case "missing right hand side in clause"
                         stx #'[(? ...)])]
    [(_ who (operand* ...)
        [(?* ...) c0 c1 ...] ...
        [bad-list . rest] . rest^)
     (not (list? (syntax->datum #'bad-list)))
     (raise-syntax-error 'operand-case 
      "left hand side of clause must be an unquoted list of predicates"
      stx #'[bad-list . rest])]
    [(_ who bad-list . rest)
     (not (list? (syntax->datum #'bad-list)))
     (raise-syntax-error 'operand-case
      "operands list must be an unquoted list of identifiers"
      stx #'bad-list)]
    [other
     (raise-syntax-error 'operand-case
      "basic syntax is (operand-case who (id* ...) [(rand? ...) rhs rhs* ...] ...)"
      stx)]))

(define-for-syntax (part-on-fst fst clauses)
  (define ((help? fst) clause)
    (syntax-case clause ()
      [[(fst^ _ ...) c] (identifier? #'fst^) (free-identifier=? fst #'fst^)]
      [other #f]))
  (if (identifier? fst)
      (partition (help? fst) (syntax->list clauses))
      (values '() clauses)))

(define-syntax (operand-case-aux stx)
  (syntax-case stx ()
    [(_ who (matched* ...) (used-pred* ...) (operand operand* ...)
        [(fst?  rest? ...)  code] c* ...)
     (let-values ([(same other) (part-on-fst #'fst? #'(c* ...))])
       (with-syntax ([([(_       s-rest?** ...) s-code*] ...) same]
                     [([(o-fst?* o-rest?** ...) o-code]  ...) other])
         (syntax/loc stx
           (if (fst? operand)
               (operand-case-aux who 
                                 (matched* ... operand) 
                                 (used-pred* ... fst?)
                                 (operand* ...)
                                 [(rest? ...)     code]
                                 [(s-rest?** ...) s-code*] ...)
               (operand-case-aux who 
                                 (matched* ...) 
                                 (used-pred* ...)
                                 (operand operand* ...)
                                 [(o-fst?* o-rest?** ...) o-code] ...)))))]
    [(_ who (m* ...) (p* ...) () [() code]) 
     (syntax/loc stx code)]
    [(_ who (m* ...) (p* ...) (a ...)) 
     (syntax/loc stx (error who "undefined for operands ~a" `(,m* ... ,a ...)))]
    [(_ who (m* ...) (p* ...) () [() code] [() code*] ...)
     (raise-syntax-error 'operand-case 
                         "overlapping cases" 
                         stx
                         #'([(p* ...) code]
                            [(p* ...) code*] ...))]
    [(_ other ...) 
     (raise-syntax-error 'operand-case 
                         "internal error in macro definition" 
                         stx)]))
