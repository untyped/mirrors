#lang scheme/base

(require "../../base.ss")

(require "syntax-hash-internal.ss")

(define join-hash     (make-syntax-hash))
(define function-hash (make-syntax-hash))

(define join-hash-ref      (cut syntax-hash-ref    join-hash <>))
(define join-hash-add!     (cut syntax-hash-add!   join-hash <> <> <> <>))
(define expand-join        (cut syntax-hash-expand join-hash <>))

(define function-hash-ref  (cut syntax-hash-ref    function-hash <>))
(define function-hash-add! (cut syntax-hash-add!   function-hash <> <> <> <>))
(define expand-function    (cut syntax-hash-expand function-hash <>))

(provide (except-out (struct-out syntax-info) make-syntax-info)
         join-hash-ref
         join-hash-add!
         join-hash-expand
         function-hash-ref
         function-hash-add!
         function-hash-expand)
