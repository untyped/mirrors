(module abstract-regexps mzscheme
  (require-for-syntax (lib "list.ss" "srfi" "1")
                      (lib "string.ss" "srfi" "13")
                      (lib "match.ss"))
  (require (lib "lex.ss" "parser-tools"))

  (define-for-syntax regexp-keywords
    '(union sequence kleene* kleene+ maybe complement range any save))

  (define-syntax (define-abstract-regexps stx)
    (define (interp x env)
      (define (interp* xs)
        (map (lambda (x)
               (interp x env))
             xs))
      (syntax-case x ()
        [(op are ...)
         (and (identifier? #'op)
              (memq (syntax-object->datum #'op) regexp-keywords))
         `(,(syntax-object->datum #'op) ,@(interp* (syntax->list #'(are ...))))]
        [datum
         (let ([v (syntax-object->datum #'datum)])
           (or (string? v) (char? v)))
         (syntax-object->datum #'datum)]
        [id
         (identifier? #'id)
         (cond
           [(s:assoc #'id env module-identifier=?) => cdr]
           [else (raise-syntax-error 'define-abstract-regexps "unbound regexp variable" x x)])]))
    (syntax-case stx ()
      [(_ [name binding] ...)
       (let loop ([names (syntax->list #'(name ...))]
                  [bindings (syntax->list #'(binding ...))]
                  [env '()])
         (if (null? names)
             (with-syntax ([(b ...) (reverse (map cdr env))])
               #'(begin (define-syntax name 'b) ...))
             (loop (cdr names)
                   (cdr bindings)
                   (cons (cons (car names) (interp (car bindings) env))
                         env))))]))

  (define-for-syntax (string-append-map c->s s)
    (string-fold-right (lambda (c rest)
                         (string-append (c->s c) rest))
                       ""
                       s))

  (define-for-syntax rx-special-chars
    '(#\? #\| #\^ #\$ #\& #\( #\) #\{ #\} #\[ #\] #\+ #\- #\* #\. #\\))

  (define-for-syntax (escape-rx c)
    (if (memq c rx-special-chars)
        (string #\\ c)
        (string c)))

  (define-for-syntax (single-char? x)
    (or (char? x)
        (and (string? x) (= (string-length x) 1))))

  (define-syntax (make-rx stx)
    ;; char-are? : are -> boolean
    (define (char-are? are)
      (match are
        [('range c1 c2) #t]
        [('union (? char-are?) ...) #t]
        [('sequence (? char-are?)) #t]
        [('any) #t]
        [(? char?) #t]
        [(? string?) (= (string-length are) 1)]
        [_ #f]))

    ;; char-are->rx : char-are -> string
    (define (char-are->rx char-are)
      (match char-are
        [('range c1 c2) (format "~a-~a" (escape-rx c1) (escape-rx c2))]
        [('union char-ares ...) (string-append-map escape-rx char-ares)]
        [('sequence char-are) (char-are->rx char-are)]
        [('any) "."]
        [(? char?) (escape-rx char-are)]
        [(? string?) (string-append-map escape-rx char-are)]
        [_ (error 'are->rx "must match exactly one character")]))

    ;; simple-are? : are -> boolean
    (define (simple-are? are)
      (match are
        [(? char-are?) #t]
        [('range c1 c2) #t]
        [('union (? simple-are?)) #t]
        [('sequence (? simple-are?)) #t]
        [('any) #t]
        [('complement char-ares ...) #t]
        [('save ares ...) #t]
        [(? char?) #t]
        [(? string?) (= (string-length are) 1)]
        [_ #f]))

    ;; are->rx : are -> string
    (define (are->rx are)
      (match are
        [('range char-are1 char-are2)
         (format "[~a-~a]" (escape-rx char-are1) (escape-rx char-are2))]
        [('kleene* are)
         (format "~a*" (are->sub-rx are))]
        [('kleene* ares ...)
         (format "(?:~a)*" (apply string-append (map are->rx ares)))]
        [('kleene+ are)
         (format "~a+" (are->sub-rx are))]
        [('kleene+ ares ...)
         (format "(?:~a)+" (apply string-append (map are->rx ares)))]
        [('union are)
         (are->rx are)]
        [('union (? char-are? ares) ...)
         (apply string-append (cons "[" (append (map char-are->rx ares) (list "]"))))]
        [('union ares ...)
         (string-join (map are->sub-rx ares) "|" 'infix)]
        [('sequence are)
         (are->rx are)]
        [('sequence ares ...)
         (apply string-append (map are->sub-rx ares))]
        [('any)
         "."]
        [('maybe are)
         (format "~a?" (are->sub-rx are))]
        [('maybe ares ...)
         (format "(?:~a)?" (apply string-append (map are->rx ares)))]
        [('complement char-ares ...)
         (format "[^~a]" (apply string-append (map char-are->rx char-ares)))]
        [('save are)
         (format "(~a)" (are->rx are))]
        [(? string?) (string-append-map escape-rx are)]
        [(? char?) (escape-rx are)]))

    (define (are->sub-rx are)
      (if (simple-are? are)
          (are->rx are)
          (format "(?:~a)" (are->rx are))))

    (syntax-case stx ()
      [(_ id)
       (identifier? #'id)
       (with-syntax ([rx (string-append "^" (are->sub-rx (syntax-local-value #'id)))])
         #'(regexp rx))]))

  (define-lex-trans make-lex
    (letrec ([are->sre (lambda (are)
                         (match are
                           [('range char-are1 char-are2)
                            (with-syntax ([c1 char-are1]
                                          [c2 char-are2])
                              #'(char-range c1 c2))]
                           [('kleene* ares ...)
                            (with-syntax ([(ares* ...) (map are->sre ares)])
                              #'(repetition 0 +inf.0 (concatenation ares* ...)))]
                           [('kleene+ ares ...)
                            (with-syntax ([(ares* ...) (map are->sre ares)])
                              #'(repetition 1 +inf.0 (concatenation ares* ...)))]
                           [('union ares ...)
                            (with-syntax ([(ares* ...) (map are->sre ares)])
                              #'(union ares* ...))]
                           [('sequence ares ...)
                            (with-syntax ([(ares* ...) (map are->sre ares)])
                              #'(concatenation ares* ...))]
                           [('any)
                            #'(any-char)]
                           [('maybe ares ...)
                            (with-syntax ([(ares* ...) (map are->sre ares)])
                              #'(repetition 0 1 (concatenation ares* ...)))]
                           [('complement char-ares ...)
                            (with-syntax ([re (apply string-append
                                                     (map (lambda (x)
                                                            (cond
                                                              [(string? x) x]
                                                              [(char? x) (string x)]
                                                              [else (raise-syntax-error
                                                                     'make-lex
                                                                     "complement can only contain chars or strings")]))
                                                          char-ares))])
                              #'(char-complement (char-set re)))]
                           [('save are)
                            (are->sre are)]
                           [(? string?)
                            (with-syntax ([s are])
                              #'s)]
                           [(? char?)
                            (with-syntax ([c are])
                              #'c)]))])
      (lambda (stx)
        (syntax-case stx ()
          [(_ id)
           (identifier? #'id)
           (are->sre (syntax-local-value #'id))]))))

  (define-syntax define-rx
    (syntax-rules ()
      [(_ name binding)
       (begin
         (define-abstract-regexps [t binding])
         (define name (make-rx t)))]))

  (provide define-abstract-regexps make-rx make-lex))
