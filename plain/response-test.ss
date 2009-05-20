#lang scheme/base

(require "../test-base.ss")

(require "../main.ss")

(define response-tests
  (test-suite "response.ss"
    
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
             (string->url "http://www.untyped.com/!@£$%^&*()#-=_+[]{};'\\:\"|,./<>?`~"))))))

; Provide statements -----------------------------

(provide response-tests)