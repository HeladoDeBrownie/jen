#lang racket
(require
  (for-syntax syntax/parse)
  "base.rkt")
(provide
 rule
 define-rule
 ~>
 define-clause-syntax/combiner)

(define-syntax rule
  (syntax-parser
    ((_
      (~seq a-clause
            (~optional (~seq #:weight weight:expr)
                       #:defaults ((weight #'1))))
      ...)
     #'(rule-struct (hash (~@ a-clause (λ () weight)) ...)))))

(define-syntax-rule
  (define-rule id rest ...)
  (define id (rule rest ...)))

(define-syntax ~>
  (syntax-parser
    ((_ expression:expr ...
        (~optional (~seq #:combiner combiner:expr)
                   #:defaults ((combiner #'~a))))
     #'(λ () (combine combiner expression ...)))))

(define-syntax define-clause-syntax/combiner
  (syntax-parser
    ((_ identifier:identifier combiner:expr)
     #'(define-syntax-rule (identifier form (... ...))
         (~> form (... ...) #:combiner combiner)))))

(define (combine combiner . values)
  (apply combiner (filter (negate void?) values)))