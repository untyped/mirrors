(module helpers mzscheme
  (require (lib "etc.ss")
           "../syntax/ast.ss"
           "../syntax/token.ss")

  ;; This syntax object will have the syntax-original? property. It can be used
  ;; with datum->syntax-object to give subsequent syntax objects this property.
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))

  ;; Identifier->syntax : Identifier -> syntax
  (define Identifier->syntax
    (opt-lambda (id [loc (Term-location id)])
      (build-syntax (Identifier-name id) loc)))

  ;; Identifier->key : Identifier -> syntax
  (define (Identifier->key id)
    (build-syntax (symbol->string (Identifier-name id))
                  (Term-location id)))

  ;; build-syntax : any [(optional region) boolean] -> syntax
  (define build-syntax
    (opt-lambda (expr [location #f] [original? #t])
      (datum->syntax-object #f
                            expr
                            (and location (region->syntax location original?))
                            (and original? stx-for-original-property))))

  ;; region->syntax : region [boolean] -> syntax
  (define region->syntax
    (opt-lambda (region [original? #t])
      (let ([start (region-start region)]
            [end (region-end region)])
        (datum->syntax-object #f
                              'source-location
                              (list
                               (region-source region)
                               (position-line start)
                               (position-col start)
                               (position-offset start)
                               (- (position-offset end) (position-offset start)))
                              (and original? stx-for-original-property)))))

  (provide (all-defined)))
