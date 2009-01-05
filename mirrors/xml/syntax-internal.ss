#lang scheme/base

(require "../base.ss")

; syntax -> boolean
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; syntax -> boolean
(define (tag-identifier? stx)
  (and (identifier? stx)
       ; Must start with a letter, an underscore or a colon:
       (regexp-match #rx"^[a-zA-Z_:]" (symbol->string (syntax->datum stx)))
       #t))

; syntax -> string
(define (tag->string stx)
  (symbol->string (syntax->datum stx)))

; syntax syntax -> void
(define (prevent-bad-tag-syntax tag-stx expr-stx)
  (unless (tag-identifier? tag-stx)
    (raise-syntax-error #f "not a valid tag name" expr-stx tag-stx))
  (when (prevent-quoting-errors?)
    (cond [(memq (syntax->datum tag-stx) '(xml xml-attrs xml* xml-attrs* opt-xml opt-xml-attr))
           (raise-syntax-error #f "xml block found in xml output" expr-stx)]
          [(memq (syntax->datum tag-stx) '(js opt-js))
           (raise-syntax-error #f "javascript block found in xml output" expr-stx)])))

; Provide statements -----------------------------

(provide/contract
 [quotable-literal?      (-> syntax? boolean?)]
 [tag-identifier?        (-> syntax? boolean?)]
 [tag->string            (-> syntax? string?)]
 [prevent-bad-tag-syntax (-> syntax? syntax? void?)])
 