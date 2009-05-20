#lang scheme/base

(require "../base.ss")

(require (for-template scheme/base
                       "struct.ss"
                       "render-internal.ss")
         (only-in srfi/1 append-map)
         (unlib-in syntax)
         "expander.ss"
         "render.ss"
         "render-internal.ss"
         "syntax-internal.ss"
         "struct.ss")

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
     (cons #'(attributes->raw expr) (prerender-attributes #'(other ...)))]
    [((unquote-splicing expr) other ...)
     (cons #'(attributes->raw expr) (prerender-attributes #'(other ...)))]
    [([name (unquote expr)] other ...)
     (xml-identifier-guard #'name stx)
     (let ([name (identifier->string #'name)])
       (append (list " " name "=\"" #'(quote-javascript-attribute-value expr) "\"") (prerender-attributes #'(other ...))))]
    [([name value] other ...)
     (xml-identifier-guard #'name stx)
     (let ([name  (identifier->string #'name)]
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
    [_             (let ([datum (syntax->datum stx)])
                     (cond [(symbol? datum)
                            (list (format "&~a;" datum))]
                           [(and (integer? datum) (>= datum 0))
                            (list (format "&#~a;" datum))]
                           [else (raise-syntax-error
                                  'mirrors/xml
                                  "bad XML entity value: expected (U symbol natural unquote-expression)"
                                  stx)]))]))

; syntax -> (listof (U syntax string))
(define (prerender-node stx)
  (syntax-case* stx (quote unquote unquote-splicing !raw !comment !cdata !pi @ &) symbolic-identifier=?
    [(expander arg ...)           (xml-expander-syntax? #'expander)
                                  (let ([expanded-stx (xml-expand #'(expander arg ...))])
                                    (syntax-case* expanded-stx (xml xml*) symbolic-identifier=?
                                      [(xml  expr ...) (append-map prerender-node (syntax->list #'(expr ...)))]
                                      [(xml* expr ...) (append-map prerender-node (syntax->list #'(expr ...)))]))]
    [(quote expr)                 (prerender-literal #'(quote expr))]
    [(quote expr ...)             (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(unquote expr)               (prerender-literal #'(unquote expr))]
    [(unquote expr ...)           (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(unquote-splicing expr)      (prerender-literal #'(unquote-splicing expr))]
    [(unquote-splicing expr ...)  (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(!raw expr ...)              (prerender-raw #'(expr ...))]
    [(!comment expr ...)          (prerender-comment #'(expr ...))]
    [(!cdata expr ...)            (prerender-cdata #'(expr ...))]
    [(!pi expr ...)               (prerender-pi #'(expr ...))]
    [(& expr)                     (prerender-entity #'expr)]
    [(& expr ...)                 (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(tag)                        (xml-identifier-guard #'tag stx)
                                  (let ([tag-str (identifier->string #'tag)])
                                    (if (preserve-singletons? (string->symbol tag-str))
                                        `("<" ,tag-str "></" ,tag-str ">")
                                        `("<" ,tag-str " />")))]
    [(tag (@ attr ...))           (xml-identifier-guard #'tag stx)
                                  (let ([tag-str (identifier->string #'tag)]
                                        [attrs   (prerender-attributes #'(attr ...))])        
                                    (if (preserve-singletons? (string->symbol tag-str))
                                        `("<" ,tag-str ,@attrs "></" ,tag-str ">")
                                        `("<" ,tag-str ,@attrs " />")))]
    [(tag (@ attr ...) child ...) (xml-identifier-guard #'tag stx)
                                  (let ([tag-str  (identifier->string #'tag)]
                                        [attrs    (prerender-attributes #'(attr ...))]
                                        [children (append-map prerender-node (syntax->list #'(child ...)))])
                                    `("<" ,tag-str ,@attrs ">" ,@children "</" ,tag-str ">"))]
    [(tag child ...)              (xml-identifier-guard #'tag stx)
                                  (let ([tag-str  (identifier->string #'tag)]
                                        [children (append-map prerender-node (syntax->list #'(child ...)))])
                                    `("<" ,tag-str ">" ,@children "</" ,tag-str ">"))]
    [([_ ...] ...)                (raise-syntax-error 'mirrors/xml "bad XML syntax" stx)]
    [expr                         (prerender-literal #'expr)]))

; syntax syntax -> syntax
(define (prerender-block stx)
  ; (U syntax string) boolean -> syntax
  (define (wrap item)
    (if (string? item)
        #`(make-raw #,(datum->syntax stx item))
        item))
  (syntax-case stx ()
    [(expr ...) 
     (let ([items (finish-prerender stx (append-map prerender-node (syntax->list #'(expr ...))))])
       (if (and (pair? items) (null? (cdr items)))
           (wrap (car items))
           #`(make-block (list #,@(map wrap items)))))]))

; Helpers ----------------------------------------

; literal -> string
(define (prerender-quoted-literal value)
  (xml->string (make-atom value)))

; literal -> string
(define (prerender-unquoted-literal value)
  (xml->string (make-raw value)))

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

(provide prerender-attributes
         finish-prerender
         prerender-block)
