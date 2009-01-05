(module operator mzscheme
  (require (lib "string.ss" "srfi" "13")
           "../syntax/ast.ss"
           "exceptions.ss"
           "value.ss")

  ;; 11.4.2
  (define (js:void v)
    (void))

  ;; 11.4.1
  (define (js:delete ref)
    (if (ref? ref)
        (delete-ref! ref)
        'true))

  ;; 11.4.3
  (define (js:typeof v)
    (cond
      [(void? v) "undefined"]
      [(null? v) "object"]
      [(symbol? v) "boolean"]
      [(string? v) "string"]
      [(function? v) "function"]
      [(object? v) "object"]))

  ;; 11.8.6
  (define (js:instanceof x y)
    (make-boolean (descendant-of? x y)))

  ;; 11.5.1
  (define (js:* x y)
    (* (value->number x)
       (value->number y)))

  ;; 11.5.2
  (define (js:/ x y)
    ;; try to lose some of Scheme's precision :)
    (let ([result (exact->inexact (/ (value->number x)
                                     (value->number y)))])
      (if (integer? result)
          (inexact->exact result)
          result)))

  ;; 11.6.1
  (define js:+
    (case-lambda
      [(x) (value->number x)]
      [(x y)
       (let ([x (value->primitive x object->number)]
             [y (value->primitive y object->number)])
         (if (or (string? x) (string? y))
             (string-append (value->string x)
                            (value->string y))
             (+ (value->number x)
                (value->number y))))]))

  ;; 11.6.2
  (define js:-
    (case-lambda
      [(x) (- (value->number x))]
      [(x y) (- (value->number x)
                (value->number y))]))

  ;; 11.5.3
  (define (js:% x y)
    (let ([dividend (value->number x)]
          [divisor (value->number y)])
      ;; TODO: there's probably an easier way to do this with the Scheme library
      (cond
        [(or (NaN? dividend) (NaN? divisor) (infinite? dividend) (zero? divisor))
         NaN]
        [(and (not (infinite? dividend)) (infinite? divisor))
         x]
        [else
         (let* ([quotient (exact->inexact (/ dividend divisor))]
                [sign (if (negative? quotient) - +)]
                [magnitude (inexact->exact (floor (abs quotient)))])
           (- dividend (* divisor (sign magnitude))))])))

  ;; 11.7.1
  (define (js:<< x y)
    (arithmetic-shift (value->int32 x)
                      (bitwise-and (value->uint32 y) #x1F)))

  ;; 11.7.2
  (define (js:>> x y)
    (arithmetic-shift (value->int32 x)
                      (- (bitwise-and (value->uint32 y) #x1F))))

  ;; 11.7.3
  (define (js:>>> x y)
    (arithmetic-shift (value->uint32 x)
                      (- (bitwise-and (value->uint32 y) #x1F))))

  ;; 11.10
  (define (js:& x y)
    (bitwise-and (value->int32 x)
                 (value->int32 y)))

  ;; 11.10
  (define (js:^ x y)
    (bitwise-xor (value->int32 x)
                 (value->int32 y)))

  ;; 11.10
  (define (js:\| x y)
    (bitwise-ior (value->int32 x)
                 (value->int32 y)))

  ;; 11.4.8
  (define (js:~ x)
    (bitwise-not (value->int32 x)))

  ;; 11.4.9
  (define (js:! x)
    (make-boolean (eq? (value->boolean x) 'false)))

  ;; 11.8.5
  (define (less-than? x y)
    (let ([x (value->primitive x object->number)]
          [y (value->primitive y object->number)])
      (if (and (string? x) (string? y))
          (and (not (string-prefix? y x))
               (string-prefix? x y))
          (< (value->number x)
             (value->number y)))))

  ;; 11.8.1
  (define (js:< x y)
    (make-boolean (less-than? x y)))

  ;; 11.8.2
  (define (js:> x y)
    (make-boolean (less-than? y x)))

  ;; 11.8.3
  (define (js:<= x y)
    (make-boolean (not (less-than? y x))))

  ;; 11.8.4
  (define (js:>= x y)
    (make-boolean (not (less-than? x y))))

  ;; 11.8.7
  (define (js:in x y)
    (unless (object? y)
      (raise-runtime-type-error here "object" (value->string y)))
    (make-boolean (has-property? y (value->string x))))

  ;; same-type? : value value -> boolean
  (define (same-type? x y)
    (or (and (void? x) (void? y))
        (and (null? x) (null? y))
        (and (symbol? x) (symbol? y))
        (and (number? x) (number? y))
        (and (string? x) (string? y))
        (and (function? x) (function? y))
        (and (object? x) (object? y))))

  ;; 11.9.3
  (define (== x y)
    (cond
      [(not (same-type? x y))
       (or (and (null? x) (void? y))
           (and (void? x) (null? y))
           (and (number? x) (string? y) (== x (value->number y)))
           (and (string? x) (number? y) (== (value->number x) y))
           (and (symbol? x) (== (value->number x) y))
           (and (symbol? y) (== x (value->number y)))
           (and (or (string? x) (number? x)) (object? y) (== x (value->primitive y)))
           (and (object? x) (or (string? y) (number? y)) (== (value->primitive x) y)))]
      [(void? x) #t]
      [(null? x) #t]
      [(number? x) (= x y)]
      [(string? x) (string=? x y)]
      [(symbol? x) (eq? x y)]
      [(object? x) (eq? x y)]))

  ;; 11.9.1
  (define (js:== x y)
    (make-boolean (== x y)))

  ;; 11.9.2
  (define (js:!= x y)
    (make-boolean (not (== x y))))

  ;; 11.9.6
  (define (=== x y)
    (and (same-type? x y)
         (or (void? x)
             (null? x)
             (and (number? x) (= x y))
             (and (string? x) (string=? x y))
             (and (symbol? x) (eq? x y))
             (and (object? x) (eq? x y)))))

  ;; 11.9.4
  (define (js:=== x y)
    (make-boolean (=== x y)))

  ;; 11.9.5
  (define (js:!== x y)
    (make-boolean (not (=== x y))))

  ;; operator->syntax : (union infix-operator prefix-operator) -> syntax
  (define (operator->syntax op)
    (if (or (infix-operator? op) (prefix-operator? op))
        (datum->syntax-object #'js:in (string->symbol (format "js:~a" op)))
        (error 'operator->syntax "bad operator")))

  (provide (all-defined-except less-than? same-type? == ===)))
