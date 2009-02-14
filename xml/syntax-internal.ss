#lang scheme/base

(require "../base.ss")

; syntax -> boolean
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; regexp
; 
; See also: http://www.w3.org/TR/REC-xml/#NT-NameStartChar
;      and: http://www.w3.org/TR/REC-xml/#NT-NameChar
(define xml-identifier-regexp
  (let* ([name-start-char (string-append ":"
                                         "A-Z"
                                         "_"
                                         "a-z"
                                         "\xC0-\xD6"
                                         "\xD8-\xF6"
                                         "\xF8-\u2FF"
                                         "\u370-\u37D"
                                         "\u37F-\u1FFF"
                                         "\u200C-\u200D"
                                         "\u2070-\u218F"
                                         "\u2C00-\u2FEF"
                                         "\u3001-\uD7FF"
                                         "\uF900-\uFDCF"
                                         "\uFDF0-\uFFFD"
                                         "\U10000-\UEFFFF")]
         [name-char       (string-append name-start-char
                                         "."
                                         "0-9"
                                         "\xB7"
                                         "\u0300-\u036F"
                                         "\u203F-\u2040"
                                         "-")])
    (regexp (format "^[~a][~a]*$" name-start-char name-char))))

; syntax -> boolean
(define (xml-identifier? stx)
  (and (identifier? stx)
       (regexp-match xml-identifier-regexp (symbol->string (syntax->datum stx)))
       (or (not (prevent-quoting-errors?))
           (not (memq (syntax->datum stx) '(xml xml-attrs xml* xml-attrs* opt-xml opt-xml-attr js opt-js))))
       #t))

; syntax -> string
(define (identifier->string stx)
  (symbol->string (syntax->datum stx)))

; Provide statements -----------------------------

(provide/contract
 [quotable-literal?      (-> syntax? boolean?)]
 [xml-identifier-regexp  regexp?]
 [xml-identifier?        (-> syntax? boolean?)]
 [identifier->string     (-> syntax? string?)])
 