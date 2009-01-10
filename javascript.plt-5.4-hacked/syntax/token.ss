(module token mzscheme
  (require (lib "struct.ss"))

  ;; We use the position struct from the parser-tools collection in order to
  ;; interoperate with the DrScheme syntax coloring mechanism, but we use our
  ;; own custom token structure since the syntax colorer doesn't make use of
  ;; the parser-tools token structure.

  (require (only (lib "lex.ss" "parser-tools")
                 position struct:position make-position position?
                 position-offset position-line position-col
                 set-position-offset! set-position-line! set-position-col!))

  ;; any * position * position
  (define-struct/properties region (source start end)
    ([prop:custom-write (lambda (r port write?)
                          (fprintf port
                                   "#<region:~a:~a-~a:~a>"
                                   (position-line (region-start r))
                                   (position-col (region-start r))
                                   (position-line (region-end r))
                                   (position-col (region-end r))))]))

  ;; region->string : region -> string
  (define (region->string r)
    (format "~a:~a:~a-~a:~a"
            (object-name (region-source r))
            (position-line (region-start r))
            (position-col (region-start r))
            (position-line (region-end r))
            (position-col (region-end r))))

  ;; string * boolean * boolean
  (define-struct regexp-contents (pattern global? case-insensitive?) #f)

  ;; symbol * (optional (union string number symbol regexp-contents)) * span
  (define-struct token (type contents location) #f)

  (provide (all-defined)
           (struct position (offset line col))))
