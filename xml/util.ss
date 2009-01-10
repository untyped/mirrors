#lang scheme/base

(require scheme/contract
         web-server/servlet
         "syntax.ss"
         "struct.ss"
         (only-in "struct-internal.ss" quotable-value?))

; xml
(define xhtml-1.0-strict-doctype
  (xml (!raw #<<ENDDOCTYPE
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

ENDDOCTYPE
             )))

; xml
(define xhtml-1.0-transitional-doctype
  (xml (!raw #<<ENDDOCTYPE
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

ENDDOCTYPE
             )))

; xml
(define xhtml-1.0-frameset-doctype
  (xml (!raw #<<ENDDOCTYPE
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">

ENDDOCTYPE
             )))

; (alistof symbol quotable-value) -> (listof attribute)
(define (alist->attributes alist)
  (map (lambda (kvp)
         (make-attribute (car kvp) (cdr kvp)))
       alist))

; Provide statements -----------------------------

(provide/contract
 [xhtml-1.0-strict-doctype       xml?]
 [xhtml-1.0-transitional-doctype xml?]
 [xhtml-1.0-frameset-doctype     xml?]
 [alist->attributes              (-> (listof (cons/c symbol? quotable-value?)) (listof attribute?))])
