#lang scheme/base

(require "../base.ss")

(require web-server/http
         web-server/servlet
         (unlib-in bytes number)
         "util.ss")

; Procedures -------------------------------------

;  [#:code      integer]
;  [#:message   (U string bytes)]
;  [#:seconds   integer]
;  [#:mime-type (U string bytes)]
;  [#:headers   (listof header)]
;  (listof (U string bytes))
; ->
;  response
(define (make-plain-response
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/plain; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/full code message seconds mime-type headers
                        (map string+bytes->content content))))

;  [#:code      integer]
;  [#:message   (U string bytes)]
;  [#:seconds   integer]
;  [#:mime-type (U string bytes)]
;  [#:headers   (listof header)]
;  (output-procedure -> any)
; ->
;  response
;
; where output-procedure : (listof (or/c bytes? string?)) -> any
(define (make-plain-response/incremental
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/plain; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         generator)
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/incremental code message seconds mime-type headers
                               (lambda (output-proc)
                                 (define (modified-output-proc val)
                                   (output-proc (map string+bytes->content val)))
                                 (generator modified-output-proc)))))

;  (U string url)
;  [#:code natural]
;  [#:message (U string bytes)]
;  [#:headers (listof header)]
; ->
;  response
(define (make-redirect-response
         url
         #:code    [code 302] 
         #:message [message #"Moved temporarily"]
         #:headers [headers no-cache-http-headers])
  (make-plain-response
   #:code     code
   #:message  message
   #:headers  (cons (make-header #"Location" (url+string->bytes url))
                    (filter (lambda (header)
                              (and (not (equal? (header-field header) #"Location"))
                                   (not (equal? (header-field header) #"location"))))
                            headers))
   (list #"Redirecting you - please wait...")))

; string string [boolean] -> (listof header)
(define (file-download-headers filename mime-type #:no-cache? [no-cache? #t])
  (let* ([content-disposition-string (format "attachment; filename=~a" filename)]
         [content-disposition-header (make-header #"Content-Disposition" (string->bytes/utf-8 content-disposition-string))]
         [content-type-header        (make-header #"Content-Type" (string->bytes/utf-8 mime-type))])
    (list* content-disposition-header
           content-type-header
           (if no-cache?
               no-cache-http-headers
               null))))

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
               #:message   (or/c string? bytes?)
               #:seconds   natural?
               #:mime-type (or/c string? bytes?)
               #:headers   (listof header?))
       response/full?)]
 [make-plain-response/incremental
  (->* (procedure?)
       (#:code natural?
               #:message   (or/c string? bytes?)
               #:seconds   natural?
               #:mime-type (or/c string? bytes?)
               #:headers   (listof header?))
       response/incremental?)]
 [make-redirect-response  
  (->* ((or/c string? url?))
       (#:code natural? 
               #:message (or/c string? bytes?)
               #:headers (listof header?))
       response/full?)]
 [file-download-headers
  (->* (string? string?)
       (#:no-cache? boolean?)
       (listof header?))])
