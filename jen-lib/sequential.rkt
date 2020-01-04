#lang racket
(require
  syntax/parse/define
  "base.rkt")
(provide
  cycle)

(define-syntax-parser cycle
  ((_ e:expr ...+)
   (let ((current-count
          (syntax-local-lift-expression #'(make-rule-parameter 0)))
         (thunks
          (syntax-local-lift-expression #'(list (thunk e) ...))))
     #`(begin0
         ((list-ref #,thunks (#,current-count)))
         (#,current-count
          (modulo (add1 (#,current-count)) (length #,thunks)))))))
