#lang s-exp "module.ss"

(require (prefix-in foo: "test2.ss"))

(function dave (a b) (+ a b))
(function noel (a b) (/ a b))