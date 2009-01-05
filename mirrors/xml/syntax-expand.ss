#lang scheme/base

(require scheme/match
         (only-in srfi/1/list append-map)
         srfi/26/cut
         (planet untyped/unlib/syntax)
         (file "../base.ss")
         (file "render-private.ss")
         (file "struct.ss")
         (for-template scheme/base
                       (file "struct.ss")
                       (file "render-private.ss")))

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
      [([name (unquote value)] other ...)  (identifier? #'name)
                                           (cons #'(unquote (make-attribute 'name value)) (loop #'(other ...)))]
      [([name value] other ...)            (identifier? #'name)
                                           (cons #`(unquote (make-attribute 'name #,(expand-literal #'value))) (loop #'(other ...)))]
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
    [val (let ([datum (syntax->datum stx)])
           (cond [(symbol? datum)  #'(make-entity 'val)]
                 [(integer? datum) (if (> datum 0)
                                       #'(make-entity val)
                                       (raise-exn exn:fail:contract
                                         (format "Expected (U symbol positive-integer unquote-expression), received ~a" datum)))]))]))

; syntax -> syntax
(define (expand-node stx)
  (syntax-case* stx (quote unquote unquote-splicing !raw !comment !cdata !pi @ &) symbolic-identifier=?
    [(quote expr)                       (expand-literal #'(quote expr))]
    [(quote expr ...)                   (raise-syntax-error #f "only one argument allowed" stx)]
    [(unquote expr)                     (expand-literal #'(unquote expr))]
    [(unquote expr ...)                 (raise-syntax-error #f "only one argument allowed" stx)]
    [(unquote-splicing expr)            (expand-literal #'(unquote-splicing expr))]
    [(unquote-splicing expr ...)        (raise-syntax-error #f "only one argument allowed" stx)]
    [(!raw expr ...)                    (expand-raw #'(expr ...))]
    [(!comment expr ...)                (expand-comment #'(expr ...))]
    [(!cdata expr ...)                  (expand-cdata #'(expr ...))]
    [(!pi expr ...)                     (expand-pi #'(expr ...))]
    [(& expr)                           (expand-entity #'expr)]
    [(& expr ...)                       (raise-syntax-error #f "only one argument allowed" stx)]
    [(tag (@ [name val] ...) child ...) (identifier? #'tag)
                                        #`(make-element 'tag
                                                        #,(expand-attributes #'([name val] ...))
                                                        #,(expand-block #'(child ...)))]
    [(tag child ...)                    (identifier? #'tag)
                                        #`(make-element 'tag null #,(expand-block #'(child ...)))]
    [expr                               (expand-literal #'expr)]))

; syntax -> syntax
(define (expand-block stx)
  (syntax-case stx ()
    [(elem)     (expand-node #'elem)]
    [(elem ...) #`(make-block (list #,@(map expand-node (syntax->list #'(elem ...)))))]))

; Helpers ----------------------------------------

; syntax -> boolean
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; syntax -> string
(define (tag->string stx)
  (symbol->string (syntax->datum stx)))

; Provide statements -----------------------------

(provide expand-block
         expand-attributes)
