(module eval mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           "syntax/parse.ss"
           "syntax/sexp.ss"
           "runtime/eval.ss"
           "runtime/namespace.ss")

  ;; eval-compiled-javascript : syntax * [javascript-namespace] -> any
  (define eval-compiled-javascript*
    (opt-lambda (stx [ns (make-javascript-namespace)])
      (eval-compiled-javascript stx ns)))

  ;; eval-javascript-string : input-source * [javascript-namespace] -> any
  (define eval-javascript-string*
    (opt-lambda (src [ns (make-javascript-namespace)])
      (eval-javascript-string src ns)))

  ;; eval-javascript : sexp * [javascript-namespace] -> any
  (define eval-javascript*
    (opt-lambda (src [ns (make-javascript-namespace)])
      (eval-javascript src ns)))

  (provide/contract (rename eval-compiled-javascript* eval-compiled-javascript ((syntax?) (namespace?) . opt-> . any))
                    (rename eval-javascript-string* eval-javascript-string ((input-source?) (namespace?) . opt-> . any))
                    (rename eval-javascript* eval-javascript ((sexp?) (namespace?) . opt-> . any))
                    [make-javascript-namespace (-> namespace?)]
                    [reset-javascript-namespace! (namespace? . -> . any)]))
