#lang scheme/base

(require "../base.ss")

(require scheme/list
         (unlib-in [log syntax])
         "expander.ss"
         "render-internal.ss"
         "syntax-internal.ss"
         "struct.ss"
         (for-template scheme/base
                       "struct.ss"
                       "render-internal.ss"))

; syntax -> syntax
(define (expand-literal stx)
  (syntax-case* stx (quote unquote unquote-splicing) symbolic-identifier=?
    [(quote expr) (identifier? #'expr) #'(make-atom (quote expr))]
    [(unquote expr)                    #'expr]
    [(unquote-splicing expr)           #'(make-block expr)]
    [expr (quotable-literal? #'expr)   #'(make-atom expr)]))

; syntax -> syntax
(define (expand-attributes stx)
  ; syntax -> (listof syntax)
  (define (loop stx)
    (syntax-case* stx (unquote unquote-splicing) symbolic-identifier=?
      [((unquote expr) other ...)          (cons #'(unquote expr) (loop #'(other ...)))]
      [((unquote-splicing expr) other ...) (cons #'(unquote-splicing expr) (loop #'(other ...)))]
      [([name (unquote value)] other ...)  (if (xml-identifier? #'name)
                                               (cons #'(unquote (make-attribute 'name value)) (loop #'(other ...)))
                                               (raise-syntax-error 'mirrors/xml "invalid XML attribute name" #'name))]
      [([name value] other ...)            (if (xml-identifier? #'name)
                                               (cons #`(unquote (make-attribute 'name #,(expand-literal #'value))) (loop #'(other ...)))
                                               (raise-syntax-error 'mirrors/xml "invalid XML attribute name" #'name))]
      [()                                  null]))
  ;; syntax
  #`(quasiquote (#,@(loop stx))))

; syntax -> syntax
(define-values (expand-raw expand-comment expand-cdata expand-pi)
  (let ([make-expander
         ; syntax -> (syntax -> syntax)
         (lambda (make-item-stx)
           (lambda (stx)
             (define args
               (map (lambda (stx)
                      (syntax-case* stx (quote unquote) symbolic-identifier=?
                        [(unquote val) #`val]
                        [(quote val)   #`(quote val)]
                        [val           (quotable-literal? #'val)
                                       #`val]))
                    (syntax->list stx)))
             (with-syntax ([make-item make-item-stx])
               #`(make-item #,@args))))])
    (values (make-expander #'make-raw)
            (make-expander #'make-comment)
            (make-expander #'make-cdata)
            (make-expander #'make-pi))))

; syntax -> syntax
(define (expand-entity stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [(unquote val) #'(make-entity val)]
    [val           (let ([datum (syntax->datum stx)])
                     (cond [(symbol? datum)
                            #'(make-entity 'val)]
                           [(and (integer? datum) (>= datum 0))
                            #'(make-entity val)]
                           [else (raise-syntax-error
                                  'mirrors/xml
                                  "bad XML entity code: expected (U symbol natural unquote-expression)"
                                  stx)]))]))

; syntax -> syntax
(define (expand-node stx)
  (syntax-case* stx (quote unquote unquote-splicing !raw !comment !cdata !pi @ &) symbolic-identifier=?
    [(expander arg ...)                 (xml-expander-syntax? #'expander)
                                        (let ([expanded-stx (xml-expand #'(expander arg ...))])
                                          (syntax-case* expanded-stx (xml xml*) symbolic-identifier=?
                                            [(xml  expr ...) (expand-block #'(expr ...))]
                                            [(xml* expr ...) (expand-block #'(expr ...))]))]
    [(quote expr)                       (expand-literal #'(quote expr))]
    [(quote expr ...)                   (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(unquote expr)                     (expand-literal #'(unquote expr))]
    [(unquote expr ...)                 (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(unquote-splicing expr)            (expand-literal #'(unquote-splicing expr))]
    [(unquote-splicing expr ...)        (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(!raw expr ...)                    (expand-raw #'(expr ...))]
    [(!comment expr ...)                (expand-comment #'(expr ...))]
    [(!cdata expr ...)                  (expand-cdata #'(expr ...))]
    [(!pi expr ...)                     (expand-pi #'(expr ...))]
    [(& expr)                           (expand-entity #'expr)]
    [(& expr ...)                       (raise-syntax-error 'mirrors/xml "bad XML syntax: one argument only" stx)]
    [(tag (@ [name val] ...) child ...) (if (xml-identifier? #'tag)
                                            #`(make-element 'tag #,(expand-attributes #'([name val] ...)) #,(expand-block #'(child ...)))
                                            (raise-syntax-error 'mirrors/xml "invalid XML tag name" stx #'tag))]
    [(tag child ...)                    (if (xml-identifier? #'tag)
                                            #`(make-element 'tag null #,(expand-block #'(child ...)))
                                            (raise-syntax-error 'mirrors/xml "invalid XML tag name" stx #'tag))]
    [([_ ...] ...)                      (raise-syntax-error 'mirrors/xml "bad XML syntax" stx)]
    [expr                               (expand-literal #'expr)]))

; syntax -> syntax
(define (expand-block stx)
  (syntax-case stx ()
    [(elem)     (expand-node #'elem)]
    [(elem ...) #`(make-block (list #,@(map expand-node (syntax->list #'(elem ...)))))]))

; Provide statements -----------------------------

(provide expand-block
         expand-attributes)
