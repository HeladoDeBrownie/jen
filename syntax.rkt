#lang racket
(require
  (for-syntax syntax/parse)
  "semantics.rkt")
(provide
  define-rule
  ~>
  define-clause-syntax/combiner
  combine-strings
  $>)

(define-syntax define-rule
  (syntax-parser
    ((_ identifier:identifier
        a-clause
        ...)
     #'(define identifier (rule (list
                                 a-clause
                                 ...))))))

(define-syntax ~>
  (syntax-parser
    ((_ expression:expr ...
        (~optional (~seq #:weight weight:exact-nonnegative-integer)
                   #:defaults ((weight #'1)))
        (~optional (~seq #:combiner combiner:expr)
                   #:defaults ((combiner #'combine-strings))))
     #'(clause (λ () (combiner expression ...))
               weight))))

(define-syntax define-clause-syntax/combiner
  (syntax-parser
    ((_ identifier:identifier combiner:expr)
     #'(define-syntax-rule (identifier form (... ...))
         (~> form (... ...) #:combiner combiner)))))

(define (combine-strings . values)
  (apply ~a (filter (negate void?) values)))

(define-clause-syntax/combiner $> begin)