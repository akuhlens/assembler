#lang racket/base

(require "framework.rkt")
(module+ test (require rackunit))

(define-instruction-encoder encode-x86-64
  [(MOV (r/m 64) (r 64)) (opcode REX.W + 89 /r)]
  [(MOV (r/m 32) (r 32)) (opcode 89 /r)]
  [(MOV (r/m 16) (r 16)) (opcode 89 /r)]
  [(MOV (r/m 64) (r 64)) (opcode REX.W + 89 /r)]
  [(ADD (r/m 64) (r 64)) (opcode REX.W + 89 /r)])

(module+ test
  (error 'encoder/test "TODO unit test every line")
  (test-equal?
   (encode-x86-64 '(MOV (register 64 RAX) (register 64 RCX)))
   '())
  (test-equal?
   (encode-x86-64 '(MOV (register 64 R8)  (register 64 R10)))
   '())
  (test-equal?
   (encode-x86-64 '(MOV (register 64 RCX) (register 64 RAX)))
   '())
  (test-equal?
   (encode-x86-64 '(FOO (register 64 RBX) (register 64 RBP)))
   '()))
