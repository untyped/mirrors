#lang scheme/base

(require "../base.ss")

(require (unlib-in bytes)
         "../plain/util.ss"
         "render.ss"
         "struct.ss")

;  [#:code      integer]
;  [#:message   (U bytes string)]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  csv
; ->
;  response
(define (make-csv-response
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/csv; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (make-response/full code 
                      (ensure-bytes message)
                      seconds
                      (ensure-bytes mime-type)
                      headers
                      (list (string->bytes/utf-8 (csv->string content)))))

; Provide statements -----------------------------

(provide/contract
 [make-csv-response (->* (csv?)
                         (#:code integer? 
                                 #:message   (or/c bytes? string?)
                                 #:seconds   integer?
                                 #:mime-type (or/c bytes? string?)
                                 #:headers   (listof header?))
                         response/full?)])
