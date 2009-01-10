(module tokenize mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "string.ss")
           (lib "class.ss")
           (lib "match.ss")
           (lib "etc.ss")
           "ast.ss"
           "cursor.ss"
           "../config.ss"
           "../exn.ss"
           "regexps.ss"
           "token.ss")

  ;; TODO:
  ;;   - interpret number and regexp literals
  ;;   - convert fail calls to fail/loc
  ;;   - get rid of fail, rename fail/loc to fail

  (define k 3)

  (define scan-newlines? (make-parameter #f))
  (define scan-infix-operator? (make-parameter #f))

  (define (digit? ch)
    (and (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         #t))

  (define (hex-digit? ch)
    (and (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                    #\A #\B #\C #\D #\E #\F
                    #\a #\b #\c #\d #\e #\f))
         #t))

  (define (oct-digit? ch)
    (and (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
         #t))

  (define single-escape-characters
    '((#\' . #\')
      (#\" . #\")
      (#\\ . #\\)
      (#\b . #\backspace)
      (#\f . #\page)
      (#\n . #\newline)
      (#\r . #\return)
      (#\t . #\tab)
      (#\v . #\vtab)))

  (define (single-escape-char? ch)
    (and (assq ch single-escape-characters)
         #t))

  (define (unescape-chars radix . chars)
    (integer->char (string->number (list->string chars) radix)))

  (define tokenizer<%>
    (interface ()
      fail                      ; string any ... -> <never>
      fail/loc                  ; region any string any ... -> <never>
      done?                     ; -> boolean
      current-token             ; -> token
      match                     ; symbol -> token
      must-match                ; symbol -> token
      peek-token                ; [nat] -> token
      peek-token/infix-operator ; [nat] -> token
      peek-token/same-line      ; -> token
      read-token                ; [nat] -> token
      read-token/infix-operator ; [nat] -> token
      read-token/same-line      ; -> token
      unread-token              ; -> any
      skip-whitespace           ; -> any
      ))

  (define tokenizer%
    (class* object% (tokenizer<%>)
      (init port [name (object-name port)])

      (define source port)
      (define filename name)
      (define cursor (make-cursor k))

      (port-count-lines! source)

      (public fail fail/loc show-state
              done? current-token
              (token:match match)
              (token:must-match must-match)
              peek-token peek-token/infix-operator peek-token/same-line
              read-token read-token/infix-operator read-token/same-line
              unread-token
              skip-whitespace)

      ;; current-position : -> position
      (define (current-position)
        (let-values ([(line col offset) (port-next-location source)])
          (make-position offset line col)))

      (define (fail/loc loc text fmt . args)
        (raise (make-exn:fail:javascript:syntax (string->immutable-string (apply format fmt args))
                                                (current-continuation-marks)
                                                this
                                                loc
                                                text)))

      (define (fail fmt . args)
        (send/apply this fail/loc #f #f fmt args))

      ;; FOR DEBUGGING:
      (define (show-state . args)
        (unless (null? args)
          (apply fprintf (current-error-port) args)
          (fprintf (current-error-port) ": "))
        (let ([upcoming (peek-string 5 0 source)])
          (fprintf (current-error-port) "~a [~v...]~n" cursor (if (eof-object? upcoming) "" upcoming))
          #f))

      ;; unescape-string : string -> string
      (define (unescape-string str)
        (let loop ([chars (string->list str)]
                   [result null])
          (match chars
            [() (list->string (reverse result))]
            [(#\\) (fail "unterminated string literal")]
            [(#\\ (and ec (? single-escape-char?)) . rest)
             (loop rest (cons (cdr (assq ec single-escape-characters)) result))]
            [(#\\ #\x (and d1 (? hex-digit?)) (and d2 (? hex-digit?)) . rest)
             (loop rest (cons (unescape-chars 16 d1 d2) result))]
            [(#\\ #\u (and d1 (? hex-digit?))
                      (and d2 (? hex-digit?))
                      (and d3 (? hex-digit?))
                      (and d4 (? hex-digit?)) . rest)
             (loop rest (cons (unescape-chars 16 d1 d2 d3 d4) result))]
            [(#\\ (and d1 (or #\0 #\1 #\2 #\3))
                  (and d2 (? oct-digit?))
                  (and d3 (? oct-digit?)) . rest)
             (loop rest (cons (unescape-chars 8 d1 d2 d3) result))]
            [(#\\ (and d1 (? oct-digit?))
                  (and d2 (? oct-digit?)) . rest)
             (loop rest (cons (unescape-chars 8 d1 d2) result))]
            [(#\\ (and d1 (? oct-digit?)) . rest)
             (loop rest (cons (unescape-chars 8 d1) result))]
            [(#\\ c . rest)
             (loop rest (cons c result))]
            [(c . rest)
             (loop rest (cons c result))])))

      ;; parse-regexp-pattern : string -> string
      (define (parse-regexp-pattern str)
        ;; TODO: implement me
        str)

      ;; done? : -> boolean
      (define (done?)
        (eq? (token-type (peek-token)) 'END))

      ;; current-token : -> (optional token)
      (define (current-token)
        (cursor-current cursor))

      ;; match : symbol -> (optional token)
      (define (token:match tt)
        (and (eq? (token-type (peek-token)) tt)
             (read-token)))

      ;; must-match : symbol -> token
      (define (token:must-match tt)
        (unless (token:match tt)
          (fail "missing ~a" (string-downcase (symbol->string tt))))
        (current-token))

      ;; skip-whitespace : -> #f
      (define (skip-whitespace)
        (let ([match (regexp-match-peek-positions #rx"^[ \t\v]+" source)])
          (when match
            (read-string (cdar match) source))
          #f))

      ;; @ : position [position] -> region
      (define @
        (opt-lambda (start [end (current-position)])
          (make-region filename start end)))

      (define-syntax within-region
        (syntax-rules ()
          [(_ type e ...)
           (let ([start (current-position)]
                 [result (begin e ...)]
                 [end (current-position)])
             (make-token type result (@ start end)))]))

      ;; advance! : nat -> any
      (define (advance! n)
        (when (positive? n)
          (set! cursor (cursor-advance cursor read-next-token))
          (advance! (sub1 n))))

      ;; peek-token/same-line : -> token
      (define (peek-token/same-line)
        (parameterize ([scan-newlines? #t])
          (peek-token)))

      ;; peek-token/infix-operator : [nat] -> token
      (define peek-token/infix-operator
        (opt-lambda ([skip 0])
          (parameterize ([scan-infix-operator? #t])
            (peek-token skip))))

      ;; peek-token : [nat] -> token
      (define peek-token
        (opt-lambda ([skip 0])
          (if (zero? skip)
              (begin0 (read-token)
                      (unread-token))
              (begin (read-token)
                     (begin0 (peek-token (sub1 skip))
                             (unread-token))))))

      ;; read-token/same-line : -> token
      (define (read-token/same-line)
        (parameterize ([scan-newlines? #t])
          (read-token)))

      ;; read-token/infix-operator : [nat] -> token
      (define read-token/infix-operator
        (opt-lambda ([skip 0])
          (parameterize ([scan-infix-operator? #t])
            (read-token skip))))

      ;; read-token : [nat] -> token
      (define read-token
        (opt-lambda ([skip 0])
          (advance! (add1 skip))
          (let ([token (cursor-current cursor)])
            (cond
              [(and (not (scan-newlines?))
                    (eq? (token-type token) 'NEWLINE))
               (read-token)]
              ;; unary operators that are synonymous with binary operators:
              [(and (not (scan-infix-operator?))
                    (prefix-operator? (token-type token))
                    (infix-operator? (token-type token)))
               ;; NOTE: This rewrapping must happen at the time of calling this
               ;;       method, because (scan-infix-operator?) may change value
               ;;       between the time the token is originally read and when
               ;;       this method is called.
               (make-token 'UNARY (token-contents token) (token-location token))]
              [else token]))))

      ;; read-next-token : -> token
      (define (read-next-token)
        (skip-whitespace)
        (cond
          [(eof-object? (peek-char source))
           (make-token 'END #f (@ (current-position) (current-position)))]
          [(regexp-match-peek-positions rx:empty source)
           => (lambda (match)
                (let ([token (within-region 'NEWLINE
                               (length (regexp-match* #rx"\n" (read-string (cdar match) source))))])
                  (if (> (token-contents token) 0)
                      token
                      (read-next-token))))]
          [(regexp-match-peek-positions rx:float source)
           ;; TODO: interpret numbers correctly
           => (lambda (match)
                (within-region 'NUMBER
                  (string->number (read-string (cdar match) source))))]
          [(regexp-match-peek-positions rx:integer source)
           => (lambda (match)
                (within-region 'NUMBER
                  (string->number (read-string (cdar match) source))))]
          [(regexp-match-peek-positions rx:identifier source)
           => (lambda (match)
                (let* ([start (current-position)]
                       [contents (read-string (cdar match) source)]
                       [sym (string->symbol contents)])
                  (if (memq sym (lexical-keywords))
                      (make-token sym sym (@ start))
                      (make-token 'ID sym (@ start)))))]
          [(regexp-match-peek-positions rx:string source)
           => (lambda (match)
                (within-region 'STRING
                  (let ([str (read-string (cdar match) source)])
                    (unescape-string (substring str 1 (- (string-length str) 1))))))]
          [(and (not (scan-infix-operator?))
                (regexp-match-peek-positions rx:regexp source))
           => (lambda (match)
                (within-region 'REGEXP
                  (let* ([str (read-string (cdar match) source)]
                         [pattern (substring str (car (list-ref match 1)) (cdr (list-ref match 1)))]
                         [flags (cond
                                  [(list-ref match 2)
                                   => (lambda (pair)
                                        (parse-regexp-pattern
                                         (substring str (car pair) (cdr pair))))]
                                  [else #f])])
                    (make-regexp-contents pattern
                                          (and flags (regexp-match #rx"g" flags) #t)
                                          (and flags (regexp-match #rx"i" flags) #f)))))]
          [(regexp-match-peek-positions #rx"^==(?:=)?" source)
           => (lambda (match)
                (let ([start (current-position)]
                      [operator (string->symbol (read-string (cdar match) source))])
                  (make-token operator operator (@ start))))]
          [(regexp-match-peek-positions rx:assignment-operator source)
           => (lambda (match)
                (within-region 'ASSIGN
                  (string->symbol (read-string (cdar match) source))))]
          [(regexp-match-peek-positions rx:operator source)
           => (lambda (match)
                (let ([start (current-position)]
                      [operator (string->symbol (read-string (cdar match) source))])
                  (make-token operator operator (@ start))))]
          [else (fail "illegal token")]))

      ;; unread-token : -> any
      (define (unread-token)
        (set! cursor (cursor-rewind cursor))
        (let ([token (cursor-current cursor)])
          (when (and token
                     (not (scan-newlines?))
                     (eq? (token-type token) 'NEWLINE))
            (unread-token))))

      (super-make-object)))

  ;; scan : string -> (-> token)
  (define (scan str)
    (let ([t (make-object tokenizer% (open-input-string str))])
      (lambda ()
        (send t read-token))))

  (provide
   tokenizer%
   (struct exn:fail:javascript ())
   (struct exn:fail:javascript:syntax (source))
   scan))
