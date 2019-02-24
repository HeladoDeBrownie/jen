#lang racket
(require
  (for-syntax syntax/parse)
  "struct.rkt")

(define-syntax define-rule
  (syntax-parser
    #:literals (~>)
    ((_ identifier:identifier
        (~> expression:expr ...
            (~optional (~seq #:weight weight:exact-positive-integer)
                       #:defaults ((weight #'1))))
        ...)
     #'(define identifier
         (rule (list
                (clause (λ () (~a expression ...))
                        weight)
                ...))))))

(define-syntax (~> a-syntax)
  (raise-syntax-error #f "not allowed as an expression" a-syntax))

(module+ main
  (define-rule start
    (~> (greeting) " :3"))

  (define-rule greeting
    (~> "hewwo" #:weight 9)
    (~> "hoi")
    (~> "this clause always backtracks" (empty) #:weight 10000))

  (define-rule empty)

  (for ((_ (in-range 100)))
    (displayln (start))))