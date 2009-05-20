#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         web-server/servlet
         (unlib-in bytes number)
         "util.ss")

; Procedures -------------------------------------

;  [#:code      integer]
;  [#:message   string]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  (listof (U string bytes))
; ->
;  response
(define (make-plain-response
         #:code      [code      200]
         #:message   [message   "OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/plain; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (make-response/full code message seconds (ensure-bytes mime-type) headers content))

;  [#:code      integer]
;  [#:message   string]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  (output-procedure -> any)
; ->
;  response
;
; where output-procedure : (listof (or/c bytes? string?)) -> any
(define (make-plain-response/incremental
         #:code      [code      200]
         #:message   [message   "OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/plain; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         generator)
  (make-response/incremental code message seconds (ensure-bytes mime-type) headers generator))

;  (U string url)
;  [#:code natural]
;  [#:message string]
;  [#:headers (listof header)]
; ->
;  response
(define (make-redirect-response url
                                #:code    [code 302] 
                                #:message [message "Moved temporarily"]
                                #:headers [headers no-cache-http-headers])
  (make-plain-response
   #:code     code
   #:message  message
   #:headers  (cons (make-header #"Location" (url+string->bytes url))
                    (filter (lambda (header)
                              (and (not (equal? (header-field header) #"Location"))
                                   (not (equal? (header-field header) #"location"))))
                            headers))
   (list "Redirecting you - please wait...")))

; Helpers ----------------------------------------

; (U url string) -> bytes
(define (url+string->bytes url)
  (string->bytes/utf-8
   (if (string? url)
       url
       (url->string url))))

; Provide statements -----------------------------

(provide/contract
 [make-plain-response
  (->* ((listof (or/c string? bytes?)))
       (#:code natural?
               #:message   string?
               #:seconds   natural?
               #:mime-type bytes?
               #:headers   (listof header?))
       ws:response/c)]
 [make-plain-response/incremental
  (->* (procedure?)
       (#:code natural?
               #:message   string?
               #:seconds   natural?
               #:mime-type bytes?
               #:headers   (listof header?))
       ws:response/c)]
 [make-redirect-response  
  (->* ((or/c string? url?))
       (#:code natural? 
               #:message string?
               #:headers (listof header?))
       ws:response/c)])
