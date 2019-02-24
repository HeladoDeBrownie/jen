#lang racket
(require
  (for-syntax syntax/parse)
  "struct.rkt")
(provide
  define-rule
  ~>)

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
        (~optional (~seq #:weight weight:exact-positive-integer)
                   #:defaults ((weight #'1))))
     #'(clause (λ () (~a expression ...))
               weight))))

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