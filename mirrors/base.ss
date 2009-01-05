#lang scheme/base
  
(require net/url
         scheme/contract
         srfi/19
         (planet untyped/unlib/debug)
         (planet untyped/unlib/exn)
         (planet untyped/unlib/time))

; Configuration --------------------------------

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

; Provide statements --------------------------- 

(provide (all-from-out net/url
                       scheme/contract
                       srfi/19
                       (planet untyped/unlib/debug)
                       (planet untyped/unlib/exn)
                       (planet untyped/unlib/time)))

(provide/contract
 [prevent-quoting-errors? (-> boolean?)]
 [current-time-format     (parameter/c (or/c string?
                                             (-> (or/c time-utc? time-tai?)
                                                 string?)))])

