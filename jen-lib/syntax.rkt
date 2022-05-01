#lang racket
(provide
 rule
 define-rule
 simple-rule
 define-simple-rule
 ~>
 define-clause-syntax/combiner)

(require
  syntax/parse/define
  "base.rkt")

(define-syntax-parser rule
  ((_
    (~seq a-clause:expr
          (~optional (~seq #:weight weight:expr)
                     #:defaults ((weight #'1))))
    ...)
   #'(rule-struct (list (cons (~@ a-clause (λ () weight))) ...))))

(define-syntax-rule (define-rule id rest ...)
  (define id
    (rule rest ...)))

(define-syntax-rule (simple-rule expr ...)
  (rule (thunk expr) ...))

(define-syntax-rule (define-simple-rule id rest ...)
  (define id
    (simple-rule rest ...)))

(define-syntax-parser ~>
  ((_ expression:expr ...
      (~optional (~seq #:combiner combiner:expr)
                 #:defaults ((combiner #'~a))))
   #'(λ () (combine combiner expression ...))))

(define-syntax define-clause-syntax/combiner
  (syntax-parser
    ((_ identifier:identifier combiner:expr)
     #'(define-syntax-rule (identifier form (... ...))
         (~> form (... ...) #:combiner combiner)))))

(define (combine combiner . values)
  (apply combiner (filter (negate void?) values)))