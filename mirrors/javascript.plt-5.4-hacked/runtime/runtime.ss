(module runtime mzscheme
  (require "exceptions.ss" "operator.ss" "value.ss")

  ;; make-frame : hash-table -> object
  (define (make-frame table)
    (build-object table))

  (provide make-frame
           (all-from "exceptions.ss")
           (all-from "operator.ss"))

  ;; from value.ss:
  (provide current-this)
  (provide ref? deref set-ref! delete-ref!)
  (provide object? object-call object-construct object-proto object-class object-table)
  (provide build-object0 object-get object-set! object-put! object-delete! object-keys object-keys* object-keys-stream)
  (provide scope-chain-get scope-chain-set! scope-chain-delete!)
  (provide call)
  (provide completion->value completion->string
           value->boolean value->string value->object value->primitive
           value->number value->integer value->int32 value->uint32 value->uint16)
  (provide true-value?)
  (provide current-completion complete!)
  (provide build-object build-array build-function)
  (provide make-arguments-object)
  (provide global-object))
