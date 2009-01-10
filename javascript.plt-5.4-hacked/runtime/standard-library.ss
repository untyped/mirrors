(module standard-library mzscheme
  (require (planet "evector.scm" ("soegaard" "evector.plt" 1 0))
           (only (lib "mred.ss" "mred") message-box)
           (lib "string.ss" "srfi" "13")
           (lib "list.ss" "srfi" "1")
           (lib "match.ss")
           (lib "math.ss")
           (lib "etc.ss")
           "../debug.ss"
           "../syntax/regexps.ss"
           "eval.ss"
           "exceptions.ss"
           "value.ss")

  (define (object-descriptor object)
    (format "[object ~a]" (object-class object)))

  (define js:print
    (build-function 1
     (lambda (args)
       (let ([print1 (lambda (x)
                       (display (value->string x)))]
             [args (evector->list args)])
         (unless (null? args)
           (print1 (car args))
           (for-each (lambda (arg)
                       (display " ")
                       (print1 arg))
                     (cdr args)))
         (newline)))))

  (define js:alert
    (build-function 1
     (lambda (args)
       (let ([args (evector->list args)])
         (when (null? args)
           (raise-runtime-exception here "not enough arguments"))
         (let* ([msg (value->string (car args))]
                [msg-padded (if (< (string-length msg) 20)
                                (string-pad-right msg 20 #\space)
                                msg)])
           (message-box "JavaScript" msg-padded #f '(ok)))
         (void)))))

  (define js:parseInt
    (build-function 2
      (lambda (args)
        (let ([string (if (>= (evector-length args) 1)
                          (evector-ref args 0)
                          (void))]
              [radix (if (>= (evector-length args) 2)
                         (evector-ref args 1)
                         (void))])
          (let* ([s (string-trim (value->string string) char-whitespace?)]
                 [r (value->int32 radix)]
                 [sign (if (char=? (string-ref s 0) #\-)
                           (begin (set! s (substring s 1)) -1)
                           1)])
            (if (or (and (not (zero? r)) (< r 2))
                    (> r 36))
                +nan.0
                (let ([r (cond
                           [(or (string-prefix? "0x" s) (string-prefix? "0X" s))
                            (set! s (substring s 2))
                            16]
                           [(string-prefix? "0" s)
                            (set! s (substring s 1))
                            8]
                           [(zero? r)
                            10]
                           [else r])])
                  (cond
                    [(regexp-match (build-integer-regexp r) s)
                     => (lambda (match)
                          (let sum ([factor 1]
                                    [total 0]
                                    [digits (map char->digit (reverse (string->list (car match))))])
                            (if (null? digits)
                                total
                                (sum (* factor r)
                                     (+ total (* (car digits) factor))
                                     (cdr digits)))))]
                    [else +nan.0]))))))))

  (define (char->digit ch)
    (cond
      [(memv ch (string->list "0123456789"))
       (- (char->integer ch) (char->integer #\0))]
      [(memv ch (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
       (- (char->integer ch) (char->integer #\A))]
      [(memv ch (string->list "abcdefghijklmnopqrstuvwxyz"))
       (- (char->integer ch) (char->integer #\a))]
      [else
       (error 'char->digit "bad digit: ~a" ch)]))

  (define (build-integer-regexp base)
    (regexp
     (cond
       [(<= base 10)
        (format "^[0-~a]+" (sub1 base))]
       [(= base 11)
        "^[0-9Aa]+"]
       [else
        (let ([last-char-index (- base 11)])
          (format "^[0-9A-~aa-~a]+"
                  (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" last-char-index)
                  (string-ref "abcdefghijklmnopqrstuvwxyz" last-char-index)))])))

  ;; 15.1.2.3
  (define js:parseFloat
    (build-function 1
      (lambda (args)
        (let ([s (string-trim (value->string (if (> (evector-length args) 0)
                                                 (evector-ref args 0)
                                                 (void)))
                              char-whitespace?)])
          (cond
            [(regexp-match rx:float s)
             => (lambda (match)
                  ;; TODO: calculate the MV according to 9.3.1
                  (string->number (car match)))]
            [else +nan.0])))))

  (define js:isNaN
    (build-function 1
      (lambda (args)
        (make-boolean (NaN? (value->number (if (> (evector-length args) 0)
                                               (evector-ref args 0)
                                               (void))))))))

  (define js:isFinite
    (build-function 1
      (lambda (args)
        (let ([x (value->number (if (> (evector-length args) 0)
                                    (evector-ref args 0)
                                    (void)))])
          (cond
            [(NaN? x) 'false]
            [(infinite? x) 'false]
            [else 'true])))))

  (define js:eval
    (build-function 1
      (lambda (args)
        (if (zero? (evector-length args))
            (void)
            (eval-javascript-string (value->string (evector-ref args 0))
                                    (current-namespace))))))

  (define (tmp:stub arity name)
    (build-function arity
      (lambda (args)
        (error name "not yet implemented"))))

  (define js:decodeURI (tmp:stub 1 'decodeURI))
  (define js:decodeURIComponent (tmp:stub 1 'decodeURIComponent))
  (define js:encodeURI (tmp:stub 1 'encodeURI))
  (define js:encodeURIComponent (tmp:stub 1 'encodeURIComponent))

  ;; 15.2.2.1
  (define (new-Object arg-vec)
    (if (or (zero? (evector-length arg-vec))
            (null? (evector-ref arg-vec 0))
            (void? (evector-ref arg-vec 0)))
        (make-object #f #f proto:Object "Object" (object-table))
        (value->object (evector-ref arg-vec 0))))

  ;; 15.3.2.1
  (define (new-Function arg-vec)
    (let ([args (evector->list arg-vec)])
      (cond
        [(null? args) (build-function 0 void)]
        [(null? (cdr args))
         (eval-function-string (format "function(){~a}" (value->string (car args)))
                               (current-namespace))]
        [else
         (eval-function-string (format "function(~a){~a}"
                                       (string-join (map value->string (drop-right args 1)) "," 'infix)
                                       (value->string (last args)))
                               (current-namespace))])))

  ;; 15.4.2.1
  (define (new-Array arg-vec)
    (let ([len (evector-length arg-vec)])
      (if (= len 1)
          (new-Array1 (evector-ref arg-vec 0))
          (build-array arg-vec))))

  ;; 15.4.2.2
  (define (new-Array1 len)
    (if (numeric? len)
        (let* ([val (numeric->number len)]
               [uint32 (value->uint32 val)])
          (if (= val uint32)
              (let ([a (build-array (evector))])
                (set-array-length! a uint32)
                a)
              (build-array (evector len))))
        (build-array (evector len))))

  ;; 15.5.2.1
  (define (new-String arg-vec)
    (let* ([value (if (zero? (evector-length arg-vec))
                      ""
                      (value->string (evector-ref arg-vec 0)))]
           [table (object-table)])
      (hash-table-put! table '<<value>> value)
      (make-object #f #f proto:String "String" table)))

  ;; 15.6.2.1
  (define (new-Boolean arg-vec)
    (let* ([value (if (zero? (evector-length arg-vec))
                      'false
                      (value->boolean (evector-ref arg-vec 0)))]
           [table (object-table)])
      (hash-table-put! table '<<value>> value)
      (make-object #f #f proto:Boolean "Boolean" table)))

  ;; 15.7.2.1
  (define (new-Number arg-vec)
    (let* ([value (if (zero? (evector-length arg-vec))
                      0
                      (value->number (evector-ref arg-vec 0)))]
           [table (object-table)])
      (hash-table-put! table '<<value>> value)
      (make-object #f #f proto:Number "Number" table)))

  (define (reset-object! object)
    (set-object-properties! object (object-table)))

  (define (reset-global-object! global)
    (reset-object! global)
    (reset-object! proto:global) ;; TODO: get this from (object-proto global) instead?
    (reset-object! proto:proto)

    (object-put! proto:global "toString" (build-function 0
                                           (lambda (arg-vec)
                                             (object-descriptor (current-this)))))
    (object-put! proto:global "hasOwnProperty" (build-function 1
                                                 (lambda (arg-vec)
                                                   (make-boolean
                                                    (has-property?/immediate (current-this)
                                                                             (get-arg arg-vec 0)))))))

  (define (reset-primitive-constructors! global)
    (for-each (lambda (ctor proto name call construct)
                (reset-object! proto)
                (reset-object! ctor)
                (set-object-call! ctor call)
                (set-object-construct! ctor construct)
                ;; 15.2.3.1, 15.3.3.1, 15.4.3.1, 15.5.3.1, 15.6.3.1, 15.7.3.1
                (object-put! ctor "prototype" proto (bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
                ;; 15, 15.2.3, 15.3.3, 15.4.3, 15.5.3, 15.6.3, 15.7.3
                (object-put! ctor "length" 1 (bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))                
                ;; 15?
                (object-put! global name ctor (bit-field DONT-ENUM? DONT-DELETE?))
                ;; 15.2.4.1, 15.3.4.1, 15.4.4.1, 15.5.4.1, 15.6.4.1, 15.7.4.1
                (object-put! proto "constructor" ctor))
              (list Object Function Array String Boolean Number)
              (list proto:Object proto:Function proto:Array proto:String proto:Boolean proto:Number)
              (list "Object" "Function" "Array" "String" "Boolean" "Number")
              (list
               ;; 15.2.1.1
               (lambda (arg-vec)
                 (if (or (zero? (evector-length arg-vec))
                         (null? (evector-ref arg-vec 0))
                         (void? (evector-ref arg-vec 0)))
                     (new-Object arg-vec)
                     (value->object (evector-ref arg-vec 0))))
               ;; 15.3.1
               new-Function
               ;; 15.4.1
               new-Array
               ;; 15.5.1
               (compose value->string get-arg0)
               ;; 15.6.1
               (compose value->boolean get-arg0)
               ;; 15.7.1
               (compose value->number get-arg0))
              (list new-Object new-Function new-Array new-String new-Boolean new-Number))
    (reset-object! Math)
    (object-put! global "Math" Math (bit-field DONT-ENUM? DONT-DELETE?)))

  (define Object-methods
    `(;; 15.2.4.2
      (toString           ,(build-function 0
                             (lambda (arg-vec)
                               (object-descriptor (current-this)))))
      ;; 15.2.4.3
      (toLocaleString     ,(build-function 0
                             (lambda (arg-vec)
                               (let ([toString (object-get (current-this) "toString")])
                                 (if (not toString)
                                     (raise-runtime-type-error here "function" "undefined")
                                     (call toString arg-vec (lambda (s1 s2)
                                                              (raise-runtime-type-error here s1 s2))))))))
      ;; 14.2.4.4
      (valueOf            ,(build-function 0
                             (lambda (arg-vec)
                               (current-this))))
      ;; 15.2.4.5
      (hasOwnProperty     ,(build-function 0
                             (lambda (arg-vec)
                               (make-boolean
                                (has-property?/immediate (current-this)
                                                         (get-arg arg-vec 0))))))
      ;; 15.2.4.6
      (isPrototypeOf      ,(tmp:stub 1 "isPrototypeOf"))
      ;; 15.2.4.7
      (propertyIsEnumerable ,(tmp:stub 1 "propertyIsEnumerable"))
      ))

  (define Function-methods
    `(;; 15.3.4.2
      (toString           ,(build-function 0
                             (lambda (arg-vec)
                               (unless (descendant-of? (current-this) proto:Function)
                                 (raise-runtime-type-error here "function" "object"))
                               ;; TODO: show function source
                               "[object Function]")))
      ;; 15.3.4.3
      (apply              ,(tmp:stub 2 "apply"))
      ;; 15.3.4.4
      (call               ,(tmp:stub 1 "call"))
      ))

  (define Array-methods
    `(;; 15.4.4.2
      (toString           ,(build-function 0
                             (lambda (arg-vec)
                               (let ([this (current-this)])
                                 ;; TODO: should actually extract elements one by one, generically
                                 (string-join (map (lambda (elt)
                                                     (if elt (value->string elt) ""))
                                                   (evector->list (array-vector this)))
                                              ","
                                              'infix)))))
      ;; 15.4.4.3
      (toLocaleString     ,(build-function 0
                             (lambda (arg-vec)
                               (let ([this (current-this)])
                                 (unless (descendant-of? this proto:Array)
                                   (raise-runtime-type-error here "array" "object"))
                                 ;; TODO: should actually extract elements one by one
                                 (string-join (map (lambda (elt)
                                                     (if elt
                                                         (invoke elt "toLocaleString" (evector) (lambda (s1 s2)
                                                                                                  (raise-runtime-type-error here s1 s2)))
                                                         ""))
                                                   (evector->list (array-vector this)))
                                              ;; TODO: use locale info to choose separator
                                              ","
                                              'infix)))))
      ;; 15.4.4.4
      (concat             ,(tmp:stub 1 "concat"))
      ;; 15.4.4.5
      (join               ,(tmp:stub 1 "join"))
      ;; 15.4.4.6
      (pop                ,(tmp:stub 0 "pop"))
      ;; 15.4.4.7
      (push               ,(tmp:stub 1 "push"))
      ;; 15.4.4.8
      (reverse            ,(tmp:stub 0 "reverse"))
      ;; 15.4.4.9
      (shift              ,(tmp:stub 0 "shift"))
      ;; 15.4.4.10
      (slice              ,(tmp:stub 2 "slice"))
      ;; 15.4.4.11
      (sort               ,(tmp:stub 1 "sort"))
      ;; 15.4.4.12
      (splice             ,(tmp:stub 2 "splice"))
      ;; 15.4.4.13
      (unshift            ,(tmp:stub 1 "unshift"))
      ))

  (define String-statics
    `(;; 15.5.3.2
      (fromCharCode       ,(build-function 1
                             (lambda (arg-vec)
                               (list->string
                                (map (compose integer->char value->uint16)
                                     (evector->list arg-vec))))))
      ))

  (define String-methods
    `(;; 15.5.4.2
      (toString           ,(build-function 0
                             (lambda (arg-vec)
                               (current-this))))
      ;; 15.5.4.3
      (valueOf            ,(tmp:stub 0 "valueOf"))
      ;; 15.5.4.4
      (charAt             ,(tmp:stub 1 "charAt"))
      ;; 15.5.4.5
      (charCodeAt         ,(tmp:stub 1 "charCodeAt"))
      ;; 15.5.4.6
      (concat             ,(tmp:stub 1 "concat"))
      ;; 15.5.4.7
      (indexOf            ,(tmp:stub 1 "indexOf"))
      ;; 15.5.4.8
      (lastIndexOf        ,(tmp:stub 1 "lastIndexOf"))
      ;; 15.5.4.9
      (localeCompare      ,(tmp:stub 1 "localeCompare"))
      ;; 15.5.4.10
      (match              ,(tmp:stub 1 "match"))
      ;; 15.5.4.11
      (replace            ,(tmp:stub 2 "replace"))
      ;; 15.5.4.12
      (search             ,(tmp:stub 1 "search"))
      ;; 15.5.4.13
      (slice              ,(tmp:stub 2 "slice"))
      ;; 15.5.4.14
      (split              ,(tmp:stub 2 "split"))
      ;; 15.5.4.15
      (substring          ,(tmp:stub 2 "substring"))
      ;; 15.5.4.16
      (toLowerCase        ,(tmp:stub 0 "toLowerCase"))
      ;; 15.5.4.17
      (toLocaleLowerCase  ,(tmp:stub 0 "toLocaleLowerCase"))
      ;; 15.5.4.18
      (toUpperCase        ,(tmp:stub 0 "toUpperCase"))
      ;; 15.5.4.19
      (toLocaleUpperCase  ,(tmp:stub 0 "toLocaleUpperCase"))
      ))

  (define Boolean-methods
    `(;; 15.6.4.2
      (toString           ,(tmp:stub 0 "toString"))
      ;; 15.6.4.3
      (valueOf            ,(tmp:stub 0 "valueOf"))
      ))

  (define Number-statics
    `(;; 15.7.3.2
      ;; TODO: fix this
      (MAX_VALUE          ,(void)               ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
      ;; 15.7.3.3
      ;; TODO: fix this
      (MIN_VALUE          ,(void)               ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
      ;; 15.7.3.4
      (NaN                +nan.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
      ;; 15.7.3.5
      (NEGATIVE_INFINITY  -inf.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
      ;; 15.7.3.6
      (POSITIVE_INFINITY  +inf.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
      ))

  (define Number-methods
    `(;; 15.7.4.2
      (toString           ,(tmp:stub 0 "toString"))
      ;; 15.7.4.3
      (toLocaleString     ,(tmp:stub 0 "toLocaleString"))
      ;; 15.7.4.4
      (valueOf            ,(tmp:stub 0 "valueOf"))
      ;; 15.7.4.5
      (toFixed            ,(tmp:stub 1 "toFixed"))
      ;; 15.7.4.6
      (toExponential      ,(tmp:stub 1 "toExponential"))
      ;; 15.7.4.7
      (toPrecision        ,(tmp:stub 1 "toPrecision"))
      ))

  ;; 15.8.1
  (define Math-static-properties
    `(;; 15.8.1.1
      (E                  ,e                    ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.2
      (LN10               ,(log 10)             ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.3
      (LN2                ,(log 2)              ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.4
      (LOG2E              ,(/ 1 (log 2))        ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.5
      (LOG10E             ,(/ 1 (log 10))       ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.6
      (PI                 ,pi                   ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.7
      (SQRT1_2            ,(sqrt 1/2)           ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ;; 15.8.1.8
      (SQRT_2             ,(sqrt 2)             ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
      ))

  ;; 15.8.2
  (define Math-static-methods
    `(;; 15.8.2.1
      (abs ,(tmp:stub 1 "abs"))
      ;; 15.8.2.2
      (acos ,(tmp:stub 1 "acos"))
      ;; 15.8.2.3
      (asin ,(tmp:stub 1 "asin"))
      ;; 15.8.2.4
      (atan ,(tmp:stub 1 "atan"))
      ;; 15.8.2.5
      (atan2 ,(tmp:stub 2 "atan2"))
      ;; 15.8.2.6
      (ceil ,(tmp:stub 1 "ceil"))
      ;; 15.8.2.7
      (cos ,(tmp:stub 1 "cos"))
      ;; 15.8.2.8
      (exp ,(tmp:stub 1 "exp"))
      ;; 15.8.2.9
      (floor ,(tmp:stub 1 "floor"))
      ;; 15.8.2.10
      (log ,(tmp:stub 1 "log"))
      ;; 15.8.2.11
      (max ,(tmp:stub 2 "max"))
      ;; 15.8.2.12
      (min ,(tmp:stub 2 "min"))
      ;; 15.8.2.13
      (pow ,(tmp:stub 2 "pow"))
      ;; 15.8.2.14
      (random ,(tmp:stub 0 "random"))
      ;; 15.8.2.15
      (round ,(tmp:stub 1 "round"))
      ;; 15.8.2.16
      (sin ,(tmp:stub 1 "sin"))
      ;; 15.8.2.17
      (sqrt ,(tmp:stub 1 "sqrt"))
      ;; 15.8.2.18
      (tan ,(tmp:stub 1 "tan"))
      ))

  (define global-properties
    `(;; 15.1.1.1
      (NaN                +nan.0                ,(bit-field DONT-ENUM? DONT-DELETE?))
      ;; 15.1.1.2
      (Infinity           +inf.0                ,(bit-field DONT-ENUM? DONT-DELETE?))
      ;; 15.1.1.3
      (undefined          ,(void)               ,(bit-field DONT-ENUM? DONT-DELETE?))
      ))

  (define global-methods
    `(;; 15.1.2.1
      (eval               ,js:eval)
      ;; 15.1.2.2
      (parseInt           ,js:parseInt)
      ;; 15.1.2.3
      (parseFloat         ,js:parseFloat)
      ;; 15.1.2.4
      (isNaN              ,js:isNaN)
      ;; 15.1.2.5
      (isFinite           ,js:isFinite)
      ;; 15.1.3.1
      (decodeURI          ,js:decodeURI)
      ;; 15.1.3.2
      (decodeURIComponent ,js:decodeURIComponent)
      ;; 15.1.3.3
      (encodeURI          ,js:encodeURI)
      ;; 15.1.3.4
      (encodeURIComponent ,js:encodeURIComponent)
      ))

  (define global-custom-properties
    `((it                 ,(void)               ,(bit-field DONT-ENUM? DONT-DELETE?))
      ))

  (define global-custom-methods
    `(;; 15
      (print              ,js:print)
      (alert              ,js:alert)
      ))

  (define (install-properties! object properties)
    (for-each (lambda (property)
                (match property
                  [(name value)
                   (object-put! object (symbol->string name) value (bit-field DONT-ENUM?))]
                  [(name value attributes)
                   (object-put! object (symbol->string name) value attributes)]))
              properties))

  (define (install-standard-library! global)
    (reset-global-object! global)
    (reset-primitive-constructors! global)

    (install-properties! global         global-properties)
    (install-properties! global         global-methods)
    (install-properties! global         global-custom-properties)
    (install-properties! global         global-custom-methods)

    (install-properties! proto:Object   Object-methods)
    (install-properties! proto:Function Function-methods)
    (install-properties! proto:Array    Array-methods)
    (install-properties! String         String-statics)
    (install-properties! proto:String   String-methods)
    (install-properties! proto:Boolean  Boolean-methods)
    (install-properties! Number         Number-statics)
    (install-properties! proto:Number   Number-methods)
    (install-properties! Math           Math-static-properties)
    (install-properties! Math           Math-static-methods)

    (current-this global)
    global)


  (provide install-standard-library! reset-global-object! reset-primitive-constructors!))
