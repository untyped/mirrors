#lang scheme/base

(require "../base.ss")

(require web-server/servlet
         "../plain/util.ss"
         "render.ss"
         "struct.ss")

;  [#:code      integer]
;  [#:message   (U string bytes)]
;  [#:seconds   integer]
;  [#:mime-type (U string bytes)]
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
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/full code message seconds mime-type headers
                        (list (string+bytes->content (csv->string content))))))

; Provide statements -----------------------------

(provide/contract
 [make-csv-response (->* (csv?)
                         (#:code integer? 
                                 #:message   (or/c string? bytes?)
                                 #:seconds   integer?
                                 #:mime-type (or/c string? bytes?)
                                 #:headers   (listof header?))
                         response/full?)])
