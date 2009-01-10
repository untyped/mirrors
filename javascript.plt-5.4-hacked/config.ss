(module config mzscheme
  (require (all-except (file "../list.plt-2.0-hacked/list.ss") any)
           (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (lib "contract.ss")
           "settings.ss")

  ;; ===========================================================================
  ;; LEXICAL KEYWORD UTILITIES
  ;; ===========================================================================

  (define default-keywords
    '(break case catch const continue
      debugger default delete do else enum
      false finally for function if instanceof in
      let
      new null return switch this throw true
      try typeof var void while with))

  ;; keyword-guard : symbol -> boolean -> any
  (define (keyword-guard kw)
    (lambda (v)
      (if v (add-keyword kw) (remove-keyword kw))))

  ;; add-keyword : symbol -> any
  (define (add-keyword kw)
    (lexical-keywords (lset-adjoin eq? (lexical-keywords) kw)))

  ;; remove-keyword : symbol -> any
  (define (remove-keyword kw)
    (lexical-keywords (filter (lambda (k)
                                (not (eq? k kw)))
                              (lexical-keywords))))

  (define current-debug-port (make-parameter (current-error-port)))

  ;; ===========================================================================
  ;; JAVASCRIPT SETTINGS
  ;; ===========================================================================

  (provide-settings (settings current-settings default-settings)

    ;; Turn on this flag to allow anonymous function expressions to appear in
    ;; SourceElement contexts. Named functions in SourceElement contexts are
    ;; always considered declarations. (Not ECMA-compliant).
    (allow-anonymous-function-source-elements? #t               boolean?)

    ;; Allow do-while statements to omit the semicolon (not ECMA-compliant)?
    (infer-do-while-semicolon?                 #f               boolean?)

    ;; Allow Mozilla-style extended catch statements with guard expressions
    ;; (not ECMA-compliant)?
    (enable-extended-catch-statements?         #f               boolean?)

    ;; Allow function declarations to appear nested inside of statements
    ;; (not ECMA-compliant)?
    (allow-nested-function-declarations?       #f               boolean?)

    ;; Turn on this flag to enable proper tail recursion.
    (proper-tail-recursion?                    #f               boolean?)

    ;; Set this to a positive integer to set an artificial limit to the number
    ;; of allowed nested function calls.
    (stack-limit                               #f               (optional/c natural-number/c))

    ;; Turn on this flag to allow `eval' to be used in contexts other than as
    ;; a function call. Uses of `eval' in any of these other contexts will
    ;; not inherit the lexical environment of their application site.
    (allow-eval-aliasing?                      #f               boolean?)

    ;; Choose from either 'standard (i.e., the standard syntax for JavaScript)
    ;; or 'sexp (an S-expression syntax).
    (code-representation                       'standard        (symbols 'standard 'sexp))

    ;; The current set of lexical keywords.
    (lexical-keywords                          default-keywords (listof symbol?))

    ;; Enable `let' expressions, which introduce a new lexical scope block
    ;; (as opposed to implicitly hoisted variables)?
    (enable-let-expressions?                   #t               boolean?
      (keyword-guard 'let))

    ;; Debugging options:
    (debug-destination                        'error-port       (symbols 'error-port 'debug-window))
    (debug-scope-resolution?                   #f               boolean?)
    (debug-unbound-references?                 #f               boolean?))

  ;; ecma-strict? : boolean -> any
  (define (ecma-strict? flag)
    (let ([non-strict? (not flag)])
      (allow-anonymous-function-source-elements? non-strict?)
      (infer-do-while-semicolon? non-strict?)
      (enable-extended-catch-statements? non-strict?)
      (allow-nested-function-declarations? non-strict?)))

  (define (default-settings? s)
    (equal? s default-settings))

  (provide/contract [current-debug-port parameter?]
                    [ecma-strict? (boolean? . -> . any)]
                    [default-settings? (settings? . -> . boolean?)]
                    [marshall-settings (settings? . -> . any)]
                    [unmarshall-settings (any/c . -> . settings?)]))
