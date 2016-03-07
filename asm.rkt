#lang racket/base

(provide (all-defined-out))

;; Generic Assembly syntax

;; instr := (op) (op rnd) (op ,dest ,src)
;; rnd dest src := mem imm reg

;; Make the order here mimic intel syntax
(struct memory (base disp scale index)
  #:prefab)

(define (immediate? x)
  (or (exact-integer? x)
      (real? x)))












