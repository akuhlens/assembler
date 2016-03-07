#lang racket

(require syntax/parse)
(provide (all-defined-out))

(define-syntax-class data-size-class
  (pattern n:exact-nonnegative-integer
           #:do [(define d (syntax->datum #'n))]
           #:when (or (eq? 8  d) (eq? 16 d)
                      (eq? 32 d) (eq? 64 d))
           #:attr datum d))

(define-syntax-class valid-gpr-size-class
  (pattern n:data-size-class
           #:do [(define d (attribute n.datum))]
           #:when (or (eq? 8  d) (eq? 16 d)
                      (eq? 32 d) (eq? 64 d))
           #:attr datum d))

(define-syntax-class single-octal-class
  (pattern n:exact-nonnegative-integer
           #:do [(define d (syntax->datum #'n))]
           #:when (and (fixnum? d) (<= 0 d 7))
           #:attr datum d))
