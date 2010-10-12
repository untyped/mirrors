#lang scheme/base

(require "../test-base.ss")

(require web-server/http
         "../main.ss")

(define/provide-test-suite response-tests
  
  (test-case "make-plain-response"
    (check-not-exn
      (cut make-plain-response
           #:message   "message"
           #:mime-type "mime-type"
           (list "Hi" #"Hi")))
    (check-not-exn
      (cut make-plain-response
           #:message   #"message"
           #:mime-type #"mime-type"
           (list "Hi" #"Hi"))))
  
  (test-case "make-plain-response/incremental"
    (check-not-exn
      (cut make-plain-response/incremental
           #:message   "message"
           #:mime-type "mime-type"
           (lambda (output)
             (output (list "Hi" #"Hi")))))
    (check-not-exn
      (cut make-plain-response/incremental
           #:message   #"message"
           #:mime-type #"mime-type"
           (lambda (output)
             (output (list "Hi" #"Hi"))))))
  
  (test-case "make-redirect-response"
    (check-not-exn
      (cut make-redirect-response
           #:message   "message"
           "http://www.untyped.com"))
    (check-not-exn
      (cut make-redirect-response
           #:message   #"message"
           (string->url "http://www.untyped.com")))
    (check-not-exn
      (cut make-redirect-response
           #:message   #"message"
           (string->url "http://www.untyped.com/!@£$%^&*()#-=_+[]{};'\\:\"|,./<>?`~"))))
  
  (test-case "file-download-headers"
    (define (expand-header header)
      (cons (header-field header)
            (header-value header)))
    (check-equal? (map expand-header (file-download-headers "download.csv" "text/csv"))
                  (map expand-header (list* (make-header #"Content-Disposition" #"attachment; filename=download.csv")
                                            (make-header #"Content-Type" #"text/csv")
                                            no-cache-http-headers)))
    (check-equal? (map expand-header (file-download-headers "script.js" "text/javascript" #:no-cache? #f))
                  (map expand-header (list* (make-header #"Content-Disposition" #"attachment; filename=script.js")
                                            (make-header #"Content-Type" #"text/javascript")
                                            null)))))
