#lang racket
(require
  (for-syntax syntax/parse)
  "base.rkt")
(provide
 define-rule
 ~>
 define-clause-syntax/combiner
 $>)

(define-syntax define-rule
  (syntax-parser
    ((_ identifier:identifier
        (~seq a-clause
              (~optional (~seq #:weight weight:expr)
                         #:defaults ((weight #'1))))
        ...)
     #'(define identifier (rule (hash (~@ a-clause (λ () weight)) ...))))))

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

(define-clause-syntax/combiner $> begin)