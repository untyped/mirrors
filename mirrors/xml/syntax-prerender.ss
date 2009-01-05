#lang scheme/base

(require (for-template scheme/base
                       (file "struct.ss")
                       (file "render-private.ss"))
         scheme/match
         (only-in srfi/1/list append-map)
         srfi/26/cut
         (planet untyped/unlib/syntax)
         (file "../base.ss")
         (file "render.ss")
         (file "render-private.ss")
         (file "struct.ss"))

; Prerenderer procedures -------------------------

; syntax -> (listof fragment)
(define (prerender-literal stx)
  (syntax-case* stx (quote unquote unquote-splicing) symbolic-identifier=?
    [(quote val)             (identifier? #'val)
                             (list (prerender-quoted-literal (syntax->datum #'val)))]
    [(unquote expr)          (list #'expr)]
    [(unquote-splicing expr) (list #'(make-block expr))]
    [val                     (quotable-literal? #'val)
                             (list (prerender-quoted-literal (syntax->datum #'val)))]))

; syntax -> (listof (U string syntax))
(define (prerender-attributes stx)
  (syntax-case* stx (unquote unquote-splicing) symbolic-identifier=?
    [((unquote expr) other ...)
     (cons #'(attribute->raw expr) (prerender-attributes #'(other ...)))]
    [((unquote-splicing expr) other ...)
     (cons #'(make-block (map attribute->raw expr)) (prerender-attributes #'(other ...)))]
    [([name (unquote expr)] other ...)
     (let ([name (tag->string #'name)])
       (append (list " " name "=\"" #'(quote-javascript-attribute-value expr) "\"") (prerender-attributes #'(other ...))))]
    [([name value] other ...)
     (let ([name  (tag->string #'name)]
           [value (prerender-literal #'value)])
       (append `(" " ,name "=\"" ,@value "\"") (prerender-attributes #'(other ...))))]
    [() null]))

; syntax -> (listof (U syntax string))
(define-values (prerender-raw prerender-comment prerender-cdata prerender-pi)
  (let ([make-prerenderer 
         ; syntax string string -> (syntax -> (listof (U syntax string))
         (lambda (make-node-stx prefix suffix)
           (lambda (stx)
             (define args 
               (append-map (lambda (stx)
                             (syntax-case* stx (quote unquote) symbolic-identifier=?
                               [(unquote val) (prerender-literal #'(unquote val))]
                               [(quote val)   (list (format "~a" (syntax->datum #'val)))]
                               [val           (quotable-literal? #'val)
                                              (list (prerender-unquoted-literal (syntax->datum #'val)))]))
                           (syntax->list stx)))
             (if (andmap string? args)
                 `(,prefix ,@args ,suffix)
                 (with-syntax ([make-node make-node-stx])
                   (list #`(make-node #,@args))))))])
    (values (make-prerenderer #'make-raw "" "")
            (make-prerenderer #'make-comment "<!--" "-->")
            (make-prerenderer #'make-cdata "<![CDATA[" "]]>")
            (make-prerenderer #'make-pi "<?" "?>"))))

; syntax -> (listof (U syntax string))
(define (prerender-entity stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [(unquote val) (list #'(make-entity val))]
    [_ (let ([datum (syntax->datum stx)])
         (cond [(symbol? datum)
                (list (format "&~a;" datum))]
               [(integer? datum)
                (if (> datum 0)
                    (list (format "&#~a;" datum))
                    (raise-exn exn:fail:contract
                      (format "Expected (U symbol natural unquote-expression), received ~a" datum)))]))]))

; syntax -> (listof (U syntax string))
(define (prerender-node stx)
  (syntax-case* stx (quote unquote unquote-splicing !raw !comment !cdata !pi @ &) symbolic-identifier=?
    [(quote expr)                 (prerender-literal #'(quote expr))]
    [(quote expr ...)             (raise-syntax-error #f "only one argument allowed" stx)]
    [(unquote expr)               (prerender-literal #'(unquote expr))]
    [(unquote expr ...)           (raise-syntax-error #f "only one argument allowed" stx)]
    [(unquote-splicing expr)      (prerender-literal #'(unquote-splicing expr))]
    [(unquote-splicing expr ...)  (raise-syntax-error #f "only one argument allowed" stx)]
    [(!raw expr ...)              (prerender-raw #'(expr ...))]
    [(!comment expr ...)          (prerender-comment #'(expr ...))]
    [(!cdata expr ...)            (prerender-cdata #'(expr ...))]
    [(!pi expr ...)               (prerender-pi #'(expr ...))]
    [(& expr)                     (prerender-entity #'expr)]
    [(& expr ...)                 (raise-syntax-error #f "only one argument allowed" stx)]
    [(tag)                        (identifier? #'tag)
                                  (let ([tag (tag->string #'tag)])
                                    (if (preserve-singletons? (string->symbol tag))
                                        `("<" ,tag "></" ,tag ">")
                                        `("<" ,tag " />")))]
    [(tag (@ attr ...))           (identifier? #'tag)
                                  (let ([tag   (tag->string #'tag)]
                                        [attrs (prerender-attributes #'(attr ...))])
                                    (if (preserve-singletons? (string->symbol tag))
                                        `("<" ,tag ,@attrs "></" ,tag ">")
                                        `("<" ,tag ,@attrs " />")))]
    [(tag (@ attr ...) child ...) (identifier? #'tag)
                                  (let ([tag      (tag->string #'tag)]
                                        [attrs    (prerender-attributes #'(attr ...))]
                                        [children (append-map prerender-node (syntax->list #'(child ...)))])
                                    `("<" ,tag ,@attrs ">" ,@children "</" ,tag ">"))]
    [(tag child ...)              (identifier? #'tag)
                                  (let ([tag      (tag->string #'tag)]
                                        [children (append-map prerender-node (syntax->list #'(child ...)))])
                                    `("<" ,tag ">" ,@children "</" ,tag ">"))]
    [expr                         (prerender-literal #'expr)]))

; syntax -> syntax
(define (prerender-block stx)
  ; (U syntax string) boolean -> syntax
  (define (wrap item for-block?)
    (if (string? item)
        (if for-block?
            #`(make-raw #,(datum->syntax stx item))
            #`(make-raw #,(datum->syntax stx item)))
        item))
  (syntax-case stx ()
    [(expr ...) 
     (let ([items (finish-prerender stx (append-map prerender-node (syntax->list #'(expr ...))))])
       (if (and (pair? items) (null? (cdr items)))
           (wrap (car items) #f)
           #`(make-block (list #,@(map (cut wrap <> #t) items)))))]))

; Helpers ----------------------------------------

; syntax -> boolean
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; literal -> string
(define (prerender-quoted-literal value)
  (xml->string (make-atom value)))

; literal -> string
(define (prerender-unquoted-literal value)
  (xml->string (make-raw value)))

; syntax -> string
(define (tag->string stx)
  (symbol->string (syntax->datum stx)))

; (listof (U syntax string)) -> (listof (U syntax string))
(define (finish-prerender stx items)
  (let loop ([items items] [accum null])
    (match items
      [(list-rest item1 item2 rest)
       (if (and (string? item1) 
                (string? item2))
           (let ([item (string-append item1 item2)])
             (loop (cons item rest)
                   accum))
           (loop (cons item2 rest)
                 (cons item1 accum)))]
      [(list-rest item rest)
       (loop rest (cons item accum))]
      [(list)
       (reverse accum)])))

; Provide statements -----------------------------

(provide prerender-block)
