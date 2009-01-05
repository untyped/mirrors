(module eval mzscheme
  (require (lib "contract.ss")
           "../compiler/compile.ss"
           "../syntax/parse.ss"
           "../syntax/sexp.ss")

  ;; eval-compiled-javascript : syntax * javascript-namespace -> any
  (define (eval-compiled-javascript stx ns)
    (parameterize ([current-namespace ns])
      (eval stx)))

  ;; eval-javascript-string : input-source * javascript-namespace -> any
  (define (eval-javascript-string src ns)
    (eval-compiled-javascript (compile-script (parse-script src)) ns))

  ;; eval-function-string : string * javascript-namespace -> any
  (define (eval-function-string src ns)
    (eval-compiled-javascript (compile-function-expression (parse-function-expression src)) ns))

  ;; TODO: get this to work with all of the missing source location information

  ;; eval-javascript : sexp * javascript-namespace -> any
  (define (eval-javascript src ns)
    (eval-compiled-javascript (compile-script (list (sexp->SourceElement src))) ns))

  (provide/contract [eval-compiled-javascript (syntax? namespace? . -> . any)]
                    [eval-javascript-string (input-source? namespace? . -> . any)]
                    [eval-function-string (input-source? namespace? . -> . any)]
                    [eval-javascript (sexp? namespace? . -> . any)]))
