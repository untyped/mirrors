#lang scheme/base

; Require/provide aliases ------------------------

(require (planet untyped/unlib:3/require))

(define-library-aliases javascript (planet dherman/javascript:8)    #:provide)
(define-library-aliases pprint     (planet dherman/pprint:4)        #:provide)
(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases spgsql     (planet schematics/spgsql:2)     #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3)         #:provide)

(require net/url
         scheme/contract
         scheme/match
         scheme/pretty
         srfi/19
         srfi/26
         web-server/servlet
         (unlib-in [debug exn time]))

; Configuration ----------------------------------

; (parameter boolean)
;
; When the return value is #t, using (xml ...) or (js ...) within an
; (xml ...) or (js ...) block results in an exception being raised.
(define (prevent-quoting-errors?) #f)

; (parameter (U string ((U time-utc time-tai) -> string)))
;
; Can be one of:
;   - a SRFI 19 style datae format string;
;   - a procedure that converts TAI and UTC time objects into strings.
(define current-time-format
  (make-parameter "~Y-~m-~d ~H:~M:~S"))

; Provide statements -----------------------------

(provide (all-from-out net/url
                       scheme/contract
                       scheme/match
                       scheme/pretty
                       srfi/19
                       srfi/26
                       web-server/servlet)
         (unlib-out [debug time exn]))

(provide/contract
 [prevent-quoting-errors? (-> boolean?)]
 [current-time-format     (parameter/c (or/c string? (-> (or/c time-utc? time-tai?) string?)))])

