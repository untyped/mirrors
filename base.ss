#lang scheme/base
 
; Require/provide aliases ------------------------

(require (planet untyped/unlib:3/require))

(define-library-aliases cce-scheme (planet cce/scheme:4:1)          #:provide)
(define-library-aliases javascript (planet dherman/javascript:9:2)  #:provide)
(define-library-aliases pprint     (planet dherman/pprint:4)        #:provide)
(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases spgsql     (planet schematics/spgsql:2)     #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3:13)      #:provide)

(require net/url
         scheme/contract
         scheme/match
         srfi/19
         srfi/26
         (unlib-in debug exn time))

; Configuration --------------------------------

; (parameter (U 'lower 'upper))
;
; Prevents double quoting errors (xml within xml and so on).
;
; Macros like xml, opt-xml and xml-attrs set this to 'lower before expansion.
; Macros like XML, OPT-XML and XML-ATTRS set this to 'upper before expansion.
(define quote-case-restriction
  (make-parameter #f))

; (listof symbol)
(define lowercase-quote-symbols
  '(xml xml-attrs xml* xml-attrs* opt-xml opt-xml-attr js opt-js))

; (listof symbol)
(define uppercase-quote-symbols
  (for/list ([lower (in-list lowercase-quote-symbols)])
    (string->symbol (string-upcase (symbol->string lower)))))

; (parameter (U string ((U time-utc time-tai) -> string)))
;
; Can be one of:
;   - a SRFI 19 style datae format string;
;   - a procedure that converts TAI and UTC time objects into strings.
(define current-time-format
  (make-parameter "~Y-~m-~d ~H:~M:~S"))

; Provide statements --------------------------- 

(provide (all-from-out net/url
                       scheme/contract
                       scheme/match
                       srfi/26)
         (unlib-out debug exn time))

(provide/contract
 [quote-case-restriction  (parameter/c (or/c 'lower 'upper))]
 [lowercase-quote-symbols (listof symbol?)]
 [uppercase-quote-symbols (listof symbol?)]
 [current-time-format     (parameter/c (or/c string? (-> date? string?)))])
