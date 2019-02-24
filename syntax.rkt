#lang racket
(require "struct.rkt")

(define-syntax (define-rule a-syntax)
  (raise-syntax-error #f "TODO" a-syntax))

(module+ main
  (define-rule start
    (~> (greeting) " :3"))

  (define-rule greeting
    (~> "hewwo" #:weight 9)
    (~> "hoi")
    (~> "this clause always backtracks" (empty)))

  (define-rule empty)

  (for ((_ (in-range 100)))
    (displayln (start))))