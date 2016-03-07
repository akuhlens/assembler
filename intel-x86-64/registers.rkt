#lang racket

(require (for-syntax racket/syntax))

(provide (all-defined-out))

(define register-map (make-hasheq))

(define (register-name? x)
  (and (hash-ref register-map x #f) #t))

(define (name->register x)
  (hash-ref register-map x #f))

(struct register (name rex-bit octal size))

(require (for-syntax syntax/parse "syntax-classes.rkt"))
(define-syntax (define-registers stx)
  (syntax-parse stx
    [(_ (n rex:boolean oct:single-octal-class size:data-size-class) ...)
     (with-syntax
       ([(%n ...)
         (map (lambda (n) (format-id n "%~a" n))
              (syntax->list #'(n ...)))])
       #'(begin
           (define %n
             (let ([tmp (register 'n rex oct size)])
               (hash-set! register-map 'n tmp)
               tmp))
           ...))]))

(define-registers
  ;; 64 bit general purpose registers (quadword)
  ;;name-rex?-oct-size
  (RAX #f 0 64) (EAX   #f 0 32)(AX    #f 0 16)(AL    #f 0 8)(AH #f 4 8)
  (RCX #f 1 64) (ECX   #f 1 32)(CX    #f 1 16)(CL    #f 1 8)(CH #f 5 8)
  (RDX #f 2 64) (EDX   #f 2 32)(DX    #f 2 16)(DL    #f 2 8)(DH #f 6 8)
  (RBX #f 3 64) (EBX   #f 3 32)(BX    #f 3 16)(BL    #f 3 8)(BH #f 7 8)
  (RSP #f 4 64) (ESP   #f 4 32)(SP    #f 4 16)(SPL   #t 4 8)
  (RBP #f 5 64) (EBP   #f 5 32)(BP    #f 5 16)(BPL   #t 5 8)
  (RSI #f 6 64) (ESI   #f 6 32)(SI    #f 6 16)(SIL   #t 6 8)
  (RDI #f 7 64) (EDI   #f 7 32)(DI    #f 7 16)(DIL   #t 7 8)
  (R8  #t 0 64) (R8D   #t 0 32)(R8W   #t 0 16)(R8L   #t 0 8)
  (R9  #t 1 64) (R9D   #t 1 32)(R9W   #t 1 16)(R9L   #t 1 8)
  (R10 #t 2 64) (R10D  #t 2 32)(R10W  #t 2 16)(R10L  #t 2 8)
  (R11 #t 3 64) (R11D  #t 3 32)(R11W  #t 3 16)(R11L  #t 3 8)
  (R12 #t 4 64) (R12D  #t 4 32)(R12W  #t 4 16)(R12L  #t 4 8)
  (R13 #t 5 64) (R13D  #t 5 32)(R13W  #t 5 16)(R13L  #t 5 8)
  (R14 #t 6 64) (R14D  #t 6 32)(R14W  #t 6 16)(R14L  #t 6 8)
  (R15 #t 7 64) (R15D  #t 7 32)(R15W  #t 7 16)(R15L  #t 7 8)
  )

(define (register-size=? x n)
  (and (register? x) (= n (register-size x))))
(define (r64? x) (register-size=? x 64))
(define (r32? x) (register-size=? x 32))
(define (r16? x) (register-size=? x 16))
(define (r8? x)  (register-size=? x 8 ))





