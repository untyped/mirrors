(module value "core-mzscheme.ss"
  (require (planet "evector.scm" ("soegaard" "evector.plt" 1 0))
           (all-except (file "../../list.plt-2.0-hacked/list.ss") empty)
           (lib "string.ss" "srfi" "13")
           (prefix s: (lib "string.ss"))
           (lib "etc.ss")
           "../syntax/ast.ss"
           "../syntax/regexps.ss"
           "exceptions.ss"
           "object.ss"
           "../debug.ss"
           "../exn.ss")

  (define current-this (make-parameter #f))

  ;; ===========================================================================
  ;; DATA DEFINITIONS AND CONSTRUCTORS
  ;; ===========================================================================

  ;; A value is one of:
  ;;  - 'true
  ;;  - 'false
  ;;  - void?
  ;;  - null?
  ;;  - number?
  ;;  - string?
  ;;  - object?

  ;; A completion is an (optional value)

  ;; A property is one of:
  ;;  - property-value
  ;;  - (cons property-value attributes)

  ;; A property-value is one of:
  ;;  - ref
  ;;  - value

  ;; An attributes is a:
  ;;  - (bit-field-of READ-ONLY? DONT-ENUM? DONT-DELETE?)

  ;; A uint32 is an exact-integer in the range [0, 2^32)
  ;; An int32 is an exact-integer in the range ???
  ;; A uint16 is an exact-integer in the range [0, 2^16)

  ;; deref : (union ref value) -> value
  (define (deref val)
    (if (ref? val)
        ((ref-get val))
        val))

  ;; set-ref! : ref value -> any
  (define (set-ref! ref val)
    ((ref-set! ref) val))

  ;; delete-ref! : ref -> any
  (define (delete-ref! ref)
    ((ref-delete! ref)))

  (define (build-object0 table proto)
    (make-object #f #f proto (object-class proto) table))

  (define NaN +nan.0)

  (define (NaN? x)
    (or (eqv? x +nan.0)
        (eqv? x -nan.0)))

  (define (infinite? x)
    (or (eqv? x +inf.0)
        (eqv? x -inf.0)))

  (define (has-attribute? p a)
    (and (pair? p)
         (bit-flag-set? (cdr p) a)))

  (define (get-arg arg-vec i)
    (if (> (evector-length arg-vec) i)
        (evector-ref arg-vec i)
        (void)))

  (define (get-arg0 arg-vec)
    (get-arg arg-vec 0))

  ;; ===========================================================================
  ;; TYPE CONVERSIONS
  ;; ===========================================================================

  ;; 9.1
  (define (value->primitive v object->primitive)
    (if (primitive? v)
        v
        (object->primitive v)))

  ;; 9.3
  (define (value->number v)
    (if (primitive? v)
        (primitive->number v)
        (primitive->number (object->number v))))

  ;; primitive->number : primitive -> number
  (define (primitive->number v)
    (cond
      [(void? v) +nan.0]
      [(null? v) 0]
      [(eq? v 'true) 1]
      [(eq? v 'false) 0]
      [(number? v) v]
      [(string? v) (string->number v)]))

  ;; number-sign : number -> (number -> number)
  (define (number-sign x)
    (if (negative? x) - +))

  ;; 9.4, 9.5, 9.6, 9.7
  (define (real->integer v)
    ((number-sign v) (inexact->exact (floor (abs v)))))

  ;; 9.4
  (define (value->integer v)
    (let ([v (value->number v)])
      (cond
        [(NaN? v) 0]
        [(or (zero? v) (infinite? v)) v]
        [else (real->integer v)])))

  ;; 9.5, 9.6, 9.7
  (define (value->finite-integer v)
    (let ([v (value->number v)])
      (if (or (NaN? v) (infinite? v) (zero? v))
          0
          (real->integer v))))

  (define 2^32 (expt 2 32))
  (define 2^31 (expt 2 31))
  (define 2^16 (expt 2 16))
  (define 2^32-1 (sub1 (expt 2 32)))

  ;; 9.5
  (define (value->int32 v)
    (let* ([i (value->finite-integer v)]
           [masked (modulo i 2^32)])
      (if (>= masked 2^31)
          (- masked 2^32)
          masked)))

  ;; 9.6
  (define (value->uint32 v)
    (modulo (value->finite-integer v) 2^32))

  ;; 9.7
  (define (value->uint16 v)
    (modulo (value->finite-integer v) 2^16))

  ;; 9.2
  (define (value->boolean x)
    (cond
      [(void? x) 'false]
      [(null? x) 'false]
      [(symbol? x) x]
      [(number? x) (make-boolean (not (or (zero? x) (NaN? x))))]
      [(string? x) (make-boolean (string=? x ""))]
      [(object? x) 'true]))

  (define (value->string/simple x)
    (if (object? x)
        (primitive->string (object->string/simple x))
        (primitive->string x)))

  (define (value->string x)
    (if (object? x)
        (primitive->string (object->string x))
        (primitive->string x)))

  (define (completion->value x)
    (if (not x) (void) x))

  (define (completion->string x)
    (if (or (not x) (void? x))
        ""
        (value->string x)))

  (define (primitive->string p)
    (cond
      [(void? p) "undefined"]
      [(null? p) "null"]
      [(eq? p 'true) "true"]
      [(eq? p 'false) "false"]
      [(number? p) (number->string p)]
      [(string? p) p]
      [else (error 'primitive->string "unrecognized primitive: ~v" p)]))

  (define (numeric? x)
    (or (number? x)
        (and (object? x) (descendant-of? x proto:Number))))

  ;; numeric->number : numeric -> number
  (define (numeric->number x)
    (if (number? x) x (hash-table-get (object-properties x) '<<value>>)))

  ;; number->string : number -> string
  (define (number->string x)
    (cond
      [(eqv? x -inf.0) "-Infinity"]
      [(eqv? x +inf.0) "Infinity"]
      [(NaN? x) "NaN"]
      [(zero? x) "0"]
      [(integer? x) (mz:number->string (inexact->exact x))]
      ;; TODO: follow 9.8.1
      [else (mz:number->string x)]))

  ;; primitive? : value -> boolean
  (define (primitive? x)
    (or (void? x)
        (null? x)
        (eq? x 'true)
        (eq? x 'false)
        (number? x)
        (string? x)))

  ;; TODO: implement according to 9.3.1
  (define (string->number x)
    (mz:string->number x))

  ;; 8.6.2.6
  ;; try : object (listof string) (-> primitive) -> primitive
  (define (try o method-names)
    (if (null? method-names)
        (raise-runtime-type-error here "object with string representation" "?")
        (let ([method (object-get o (car method-names))])
          (cond
            [(and method (object? method) (object-call method))
             => (lambda (f)
                  (let ([result (parameterize ([current-this o])
                                  (f (evector)))])
                    (if (primitive? result)
                        (primitive->string result)
                        (try o (cdr method-names)))))]
            [else (try o (cdr method-names))]))))

  (define (object->string/simple o)
    "object")

  ;; 8.6.2.6, 9.1, 9.8
  ;; object->string : object -> primitive
  (define (object->string o)
    (try o '("toString" "valueOf")))

  ;; 8.6.2.6
  ;; object->number : object -> primitive
  (define (object->number o)
    (try o '("valueOf" "toString")))

  ;; 9.9
  (define (value->object v)
    (cond
      [(void? v) (raise-runtime-type-error here "defined value" "undefined")]
      [(null? v) (raise-runtime-type-error here "non-null value" "null")]
      [(symbol? v) ((object-construct Boolean) (evector v))]
      [(number? v) ((object-construct Number) (evector v))]
      [(string? v) ((object-construct String) (evector v))]
      [(object? v) v]
      [else (error 'value->object "unexpected non-value: ~v" v)]))

  (define (value->string/debug v)
    (cond
      [(string? v) (string->source-string v)]
      [(object? v) (object->string/debug v)]
      [else (value->string v)]))

  (define (object->string/debug o)
    (object->string/debug/immediate o))

  (define (object->string/debug/immediate o)
    (string-append "{"
                   (string-join (map (lambda (key)
                                       (format "~a:~a"
                                               key
                                               (value->string/debug (object-get o key))))
                                     (object-keys o))
                                ","
                                'infix)
                   "}"))

  ;; ===========================================================================
  ;; ARRAY INDICES
  ;; ===========================================================================

  ;; set-array-length! : array value -> any
  (define (set-array-length! a x)
    (value->array-index x
                        (lambda (length string?)
                          (set-evector-length! (array-vector a) length))
                        (lambda (string)
                          ;; TODO: range error
                          (raise-runtime-type-error here "array index" string))))

  ;; array-index? : any -> boolean
  (define (array-index? x)
    (and (integer? x)
         ;; 15.4
         (<= 0 x 2^32-1)))

  ;; A success continuation takes the successfully parsed array index and
  ;; a string representation of the array index (if the string has been
  ;; computed yet) and computes a result.

  ;; A failure continuation takes the string representation of the array
  ;; index and computes a result.

  ;; value->array-index : value (uint32 (optional string) -> a) (string -> a) -> a
  (define (value->array-index x sk fk)
    (cond
      [(array-index? x) (sk (inexact->exact x) #f)]
      [(number? x) (fk (number->string x))]
      [else
       (let ([s (value->string x)])
         (cond
           [(parse-array-index s)
            => (lambda (index)
                 (sk index s))]
           [else (fk s)]))]))

  ;; parse-array-index : string -> (optional uint32)
  (define (parse-array-index s)
    (and (s:regexp-match-exact? rx:integer s)
         (let ([i (string->number s)])
           (and (array-index? i)
                (string=? (number->string i) s)
                (inexact->exact i)))))

  ;; ===========================================================================
  ;; OBJECT PROPERTIES
  ;; ===========================================================================

  ;; property->value : property -> value
  (define (property->value p)
    (cond
      [(and (pair? p) (ref? (car p)))
       (deref (car p))]
      [(pair? p)
       (car p)]
      [(ref? p)
       (deref p)]
      [else p]))

  ;; has-property? : object string -> boolean
  (define (has-property? o key)
    (or (has-property?/immediate o key)
        (let ([proto (object-proto o)])
          (and proto (has-property? proto key)))))

  ;; has-property?/immediate : object string -> boolean
  (define (has-property?/immediate o key)
    (or (and (array? o) (array-has-property?/immediate o key))
        (object-has-property?/immediate o key)))

  ;; array-has-property?/immediate : array string -> boolean
  (define (array-has-property?/immediate a key)
    (value->array-index key
                        (lambda (index string?)
                          (let ([vec (array-vector a)])
                            (and (< index (evector-length vec))
                                 (evector-ref vec index)
                                 #t)))
                        (lambda (string) #f)))

  ;; object-has-property?/immediate : object string -> boolean
  (define (object-has-property?/immediate o key)
    (hash-table-contains? (object-properties o) key))

  ;; object-get : object value -> (optional value)
  (define (object-get o key)
    (object-get1 o key (lambda (string)
                         (let ([proto (object-proto o)])
                           (and proto (object-get proto string))))))

  ;; object-get1 : object value (string -> a) -> (union value a)
  (define (object-get1 o key fk)
    (if (array? o)
        (array-get1 o key fk)
        (object-table-get (object-properties o) key fk)))

  ;; array-get1 : array value (string -> a) -> (union value a)
  (define (array-get1 a key fk)
    (value->array-index key
                        (lambda (index string?)
                          (let ([vec (array-vector a)])
                            (cond
                              [(and (< index (evector-length vec))
                                    (evector-ref vec index))
                               => property->value]
                              [else (fk (or string? (number->string index)))])))
                        (lambda (string)
                          (object-table-get (object-properties a)
                                            string
                                            fk))))

  ;; object-table-get : hash-table value (string -> a) -> (union value a)
  (define (object-table-get table key fk)
    (let* ([s (value->string key)]
           [v (hash-table-get table key (lambda () #f))])
      (or (and v (property->value v))
          (fk s))))

  ;; object-put! : object value value [attributes] -> any
  (define object-put!
    (opt-lambda (o key value [attributes empty-bit-field])
      (if (array? o)
          (array-put! o key value attributes)
          (object-table-put! o (value->string key) value attributes))))

  ;; array-put! : array value value -> any
  (define array-put!
    (opt-lambda (a key value [attributes empty-bit-field])
      (value->array-index key
                          (lambda (index string?)
                            (array-vector-put! a index value attributes))
                          (lambda (string)
                            (object-table-put! a string value attributes)))))

  ;; put!/permission : (optional property) (property -> any) value bit-field -> any
  (define (put!/permission previous put! value attributes)
    (unless (has-attribute? previous READ-ONLY?)
      (cond
        [(and (pair? previous) (ref? (car previous)))
         (set-ref! (car previous) value)]
        [(pair? previous)
         (printf "x ~s ~s~n" previous value)
         #;(set-car! previous value)
         #;(put! (cons value (cdr previous)))]
        [(ref? previous)
         (set-ref! previous value)]
        [previous
         (put! value)]
        [(not (empty-bit-field? attributes))
         (put! (cons value attributes))]
        [else
         (put! value)])))

  ;; array-vector-put! : array uint32 value -> any
  (define array-vector-put!
    (opt-lambda (a index value [attributes empty-bit-field])
      (let ([vec (array-vector a)])
        (put!/permission (and (< index (evector-length vec))
                              (evector-ref vec index))
                         (lambda (p)
                           (evector-set! vec index p))
                         value
                         attributes))))

  ;; object-table-put! : object string value -> any
  (define object-table-put!
    (opt-lambda (o key value [attributes empty-bit-field])
      (put!/permission (hash-table-get (object-properties o) key (lambda () #f))
                       (lambda (p)
                         (hash-table-put! (object-properties o) key p))
                       value
                       attributes)))

  ;; object-delete! : object string -> (union 'true 'false)
  (define (object-delete! o key)
    (if (array? o)
        (array-delete! o key)
        (object-table-delete! (object-properties o) key)))

  ;; array-delete! : array string -> (union 'true 'false)
  (define (array-delete! a key)
    (value->array-index key
                        (lambda (index string?)
                          (array-vector-delete! (array-vector a) index))
                        (lambda (string)
                          (object-table-delete! (object-properties a) key))))

  ;; object-table-delete! : hash-table string -> (union 'true 'false)
  (define (object-table-delete! table key)
    (cond
      [(hash-table-get table key (lambda () #f))
       => (lambda (p)
            (if (has-attribute? p DONT-DELETE?)
                'false
                (begin (hash-table-remove! table key)
                       'true)))]
      [else 'true]))
        
  ;; array-vector-delete! : evector uint32 -> (union 'true 'false)
  (define (array-vector-delete! vec i)
    (cond
      [(and (<= i (evector-length vec))
            (evector-ref vec i))
       => (lambda (p)
            (if (has-attribute? p DONT-DELETE?)
                'false
                (begin (evector-set! vec i #f)
                       'true)))]
      [else 'true]))

  ;; TODO: check this against the spec for compliance
  (define (descendant-of? x y)
    (and (object? x)
         (let ([proto (object-proto x)])
           (or (eq? proto y)
               (and proto (descendant-of? proto y))))))

  ;; ===========================================================================
  ;; FOR-IN LOOPS
  ;; ===========================================================================

  (define (hash-table-contains? t key)
    (and (hash-table-get t key (lambda () #f))
         #t))

  ;; TODO: optionally catch new keys that come into existence? (hard)

  (define (object-keys-stream object)
    (let ([current-object object]
          [current-keys (object-keys object)]
          [visited (make-hash-table 'equal)])
      (letrec ([next-key (lambda ()
                           (cond
                             [(pair? current-keys)
                              (let ([key (begin0 (car current-keys)
                                                 (set! current-keys (cdr current-keys)))])
                                (if (and (not (hash-table-contains? visited key))
                                         (has-property?/immediate object key)
                                         (not (has-attribute? (hash-table-get (object-properties object) key)
                                                              DONT-ENUM?)))
                                    (begin (hash-table-put! visited key #t)
                                           key)
                                    (next-key)))]
                             [(and current-object (null? current-keys))
                              (set! current-object (object-proto current-object))
                              (set! current-keys (and current-object (object-keys current-object)))
                              (next-key)]
                             [else #f]))])
        next-key)))

  ;; object-keys* : object -> (listof string)
  (define (object-keys* o)
    (let ([next-key (object-keys-stream o)])
      (let loop ([acc '()])
        (cond
          [(next-key) => (lambda (key)
                           (loop (cons key acc)))]
          [else (reverse acc)]))))

  ;; object-keys : object -> (listof string)
  (define (object-keys o)
    (append (if (array? o)
                (build-list (evector-length (array-vector o))
                            number->string)
                null)
            (hash-table-map (object-properties o)
                            (lambda (key value) key))))

  ;; ===========================================================================
  ;; BOOLEANS
  ;; ===========================================================================

  (define (true-value? x)
    (or (object? x)
        (and (primitive? x)
             (not (or (eq? x 'false)
                      (void? x)
                      (null? x)
                      (and (number? x) (zero? x))
                      (and (string? x) (string=? x "")))))))

  (define (make-boolean b)
    (if b 'true 'false))

  ;; ===========================================================================
  ;; FUNCTIONS
  ;; ===========================================================================

  ;; invoke : value string evector (string string -> <never>) -> any
  (define (invoke v name args err)
    (let* ([this (value->object v)]
           [method (object-get this name)])
      (unless method
        (raise-runtime-type-error here "function" "undefined"))
      (parameterize ([current-this this])
        (call method args err))))

  ;; call : value evector (string string -> <never>) -> any
  (define (call v args err)
    (let* ([o (value->object v)]
           [proc (object-call o)])
      (if proc
          (proc args)
          (err "function" (value->string/simple v)))))

  ;; ===========================================================================
  ;; COMPLETIONS
  ;; ===========================================================================

  (define current-completion (make-parameter #f))

  ;; complete! : value -> completion
  (define (complete! v)
    (when v (current-completion v))
    (current-completion))

  ;; ===========================================================================
  ;; CONVENIENCE CONSTRUCTORS
  ;; ===========================================================================

  (define (build-object table)
    (build-object0 table proto:Object))

  ;; TODO: join nested function objects

  (define (build-function arity proc)
    (letrec ([f (make-object ;; 13.2.1
                             proc
                             ;; 13.2.2
                             (lambda (arg-vec)
                               (let* ([proto (or (object-get f "prototype") proto:Object)]
                                      [new-object (build-object0 '() proto)])
                                 (parameterize ([current-this new-object])
                                   (proc arg-vec))
                                 new-object))
                             proto:Function
                             "Function"
                             (object-table
                              ;; 13.2, 15.3.5.1
                              [length arity (DONT-DELETE? READ-ONLY? DONT-ENUM?)]
                              ;; 13.2, 15.3.5.2
                              [prototype (build-object (object-table [constructor f (DONT-ENUM?)]))
                                         (DONT-DELETE?)]))])
      f))

  (define (build-array vec)
    (letrec ([a (make-array #f
                            #f
                            proto:Array
                            "Array"
                            (object-table
                             [constructor Array (DONT-ENUM? DONT-DELETE?)]
                             [length (lambda ()
                                       (evector-length vec))
                                     (lambda (v)
                                       (set-array-length! a v))
                                     ;; 15.4.5.2
                                     (DONT-ENUM? DONT-DELETE?)])
                            vec)])
      a))

  ;; 11.2.1
  ;; object-set! : object value value -> value
  (define (object-set! object key value)
    (if (array? object)
        (value->array-index key
                            (lambda (index string?)
                              (evector-set! object index value)
                              value)
                            (lambda (string)
                              (object-put! object key value)))
        (object-put! object key value)))

  ;; TODO: obsolete (once I rewrite make-activation-object)
  ;; make-array-ref : evector uint32 -> ref
  (define (make-array-ref array i)
    (make-ref (lambda ()
                (if (<= i (evector-length array))
                    (evector-ref array i)
                    (void)))
              (lambda (val)
                (evector-set! array i val)
                val)
              (lambda ()
                (cond
                  [(and (<= i (evector-length array))
                        (evector-ref array i))
                   => (lambda (p)
                        (if (has-attribute? p DONT-DELETE?)
                            'false
                            (begin (evector-set! array i #f)
                                   'true)))]
                  [else 'true]))))

  (define (string->source-string v)
    (string-append "'"
                   (apply string-append
                          (map (lambda (ch)
                                 (case ch
                                   [(#\newline) "\\n"]
                                   [(#\') "\\'"]
                                   [(#\return) "\\r"]
                                   ;; TODO: etc etc
                                   [else (string ch)]))
                               (string->list v)))
                   "'"))

  ;; scope-chain-get : (listof object) string -> (optional value)
  (define (scope-chain-get scope-chain name)
    (and (pair? scope-chain)
         (or (object-get (car scope-chain) name)
             (scope-chain-get (cdr scope-chain) name))))

  ;; scope-chain-set! : (listof object) string value -> value
  (define (scope-chain-set! scope-chain name val)
    (if (has-property? (car scope-chain) name)
        (object-put! (car scope-chain) name val)
        (scope-chain-set! (cdr scope-chain) name val)))

  ;; scope-chain-delete! : (listof object) string -> (union 'true 'false)
  (define (scope-chain-delete! scope-chain name)
    (cond
      [(null? scope-chain)
       'false]
      [(has-property? (car scope-chain) name)
       (object-delete! (car scope-chain) name)]
      [else
       (scope-chain-delete! (cdr scope-chain) name)]))

  ;; 10.1.6
  ;; TODO: reimplement without make-array-ref
  (define (make-activation-object ids arguments)
    (let ([o (make-object #f
                          #f
                          proto:Object
                          "Object"
                          (object-table [arguments arguments (DONT-DELETE?)]))]
          [arg-vec (array-vector arguments)])
      (for-each (lambda (id i)
                  (object-put! o
                               (symbol->string (Identifier-name id))
                               (make-array-ref arg-vec i)
                               (bit-field DONT-DELETE?)))
                ids
                (iota ids))
      o))

  ;; 10.1.8
  (define (make-arguments-object f vec)
    (make-array #f
                #f
                proto:Object
                "Object"
                (object-table [length (evector-length vec) (DONT-ENUM?)]
                              [callee f (DONT-ENUM?)])
                vec))

  ;; ===========================================================================
  ;; CORE OBJECTS OF STANDARD LIBRARY
  ;; ===========================================================================

  ;; INVARIANT: all these uninitialized property tables are initialized by the reset-* functions

  (define proto:global
    (make-object #f #f #f "Object" #f))

  ;; TODO: give this guy his own toString and hasOwnProperty (and what else?)
  (define proto:proto
    (make-object #f #f #f "Object" #f))


  ;; 10.1.5
  (define global-object
    (make-object #f #f proto:global "DrScheme" #f))

  (define proto:Array
    (make-object #f #f proto:proto "Array" #f))
  (define proto:Function
    (make-object void void proto:proto "Function" #f))
  (define proto:Object
    (make-object #f #f proto:proto "Object" #f))
  (define proto:String
    (make-object #f #f proto:proto "String" #f))
  (define proto:Boolean
    (make-object #f #f proto:proto "Boolean" #f))
  (define proto:Number
    (make-object #f #f proto:proto "Number" #f))

  (define (make-primitive-constructor)
    (make-object #f #f proto:Function "Function" #f))

  (define Object   (make-primitive-constructor))
  (define Function (make-primitive-constructor))
  (define Array    (make-primitive-constructor))
  (define String   (make-primitive-constructor))
  (define Boolean  (make-primitive-constructor))
  (define Number   (make-primitive-constructor))
  (define Math     (make-object #f #f proto:Object "Math" #f))

  ;; ===========================================================================

  (provide current-this)
  (provide bit-field make-bit-field bit-flag-set?)
  (provide READ-ONLY? DONT-ENUM? DONT-DELETE?)
  (provide (struct object (call construct proto class properties))
           (struct array (vector))
           function?
           ref? set-ref! delete-ref! deref)
  (provide set-array-length!)
  (provide get-arg get-arg0)
  (provide object-table build-object0)
  (provide has-property? has-property?/immediate has-attribute? object-get object-set! object-put! object-delete! object-keys object-keys* object-keys-stream descendant-of?)
  (provide scope-chain-get scope-chain-set! scope-chain-delete!)
  (provide NaN NaN? infinite? numeric?)
  (provide object->number object->string object->string/simple
           completion->value completion->string
           value->boolean value->string value->string/simple value->object value->primitive
           value->number value->integer value->int32 value->uint32 value->uint16
           numeric->number)
  (provide make-boolean true-value?)
  (provide invoke call)
  (provide current-completion complete!)
  (provide build-object build-function build-array)
  (provide make-arguments-object)
  (provide global-object proto:global proto:proto proto:Array proto:Function proto:Object proto:String proto:Boolean proto:Number)
  (provide Array Function Object String Boolean Number Math))
