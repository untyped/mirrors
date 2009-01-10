(module debug mzscheme
  (require (planet "text.ss" ("dherman" "widgets.plt" 2 0))
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "match.ss")
           "config.ss")

  (define debug-frame%
    (class (frame:basic-mixin frame%)
      (init label (parent #f) (width 640) (height 480))
      (inherit show get-area-container)
      (super-new (label label)
                 (parent parent)
                 (width width)
                 (height height))

      (define-values (input-port output-port)
        (make-pipe #f 'debug 'debug))

      (define read-thread
        (thread (lambda ()
                  (let loop ()
                    (let ([in (read-line input-port)])
                      (unless (eof-object? in)
                        (append (format "~a~n" in))
                        (loop)))))))

      (define/public (append str)
        (send editor insert/programmatic str (send editor last-position)))
      (define/public (get-debug-port) output-port)

      (define contents (instantiate editor-canvas% ((get-area-container))))
      (define editor (instantiate read-only-text% ()))

      (define/public (kill)
        (show #f)
        (kill-thread read-thread))

      (send contents set-editor editor)))

  (define (create-debug-window)
    (instantiate debug-frame% ("JavaScript Debug Console")))

  (define (observing? topic)
    (case topic
      [(scope-resolution) (debug-scope-resolution?)]
      [(unbound-reference) (debug-unbound-references?)]
      [else (error 'debug (format "unknown topic: ~a" topic))]))

  (define (debug . args)
    (match args
      [((and topic (? symbol?)) (and fmt (? string?)) . rest-args)
       (when (observing? topic)
         (parameterize ([print-struct #t])
           (apply fprintf (current-debug-port) (string-append "~a: " fmt "~n") topic rest-args)))]
      [((and fmt (? string?)) . rest-args)
       (apply fprintf (current-debug-port) (string-append "DEBUG: " fmt "~n") rest-args)]))

;  (define (debug topic fmt . args)
;    (when (observing? topic)
;      (parameterize ([print-struct #t])
;        (apply fprintf (current-debug-port) (string-append "~a: " fmt "~n") topic args))))

  (provide debug create-debug-window))
