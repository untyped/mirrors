#lang scheme/base

(require scheme/contract
         (file "struct.ss"))

; Procedures -------------------------------------

; javascript-array -> (listof javascript-expression)
(define (javascript-array->list arr)
  (ArrayLiteral-elements arr))

; javascript-object -> (alistof (U symbol string number) javascript-expression)
(define (javascript-object->alist obj)
  (map (lambda (kvp)
         (let ([key (car kvp)]
               [val (cdr kvp)])
           (cons (cond [(Identifier? key)     (Identifier-name key)]
                       [(StringLiteral? key)  (StringLiteral-value key)]
                       [(NumericLiteral? key) (NumericLiteral-value key)])
                 val)))
       (ObjectLiteral-properties obj)))

; Provide statements -----------------------------

(provide/contract
 [javascript-array->list   (-> ArrayLiteral? (listof Expression?))]
 [javascript-object->alist (-> ObjectLiteral? (listof (cons/c (or/c symbol? string? number?) Expression?)))])