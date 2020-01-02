#lang racket
(require
  "base.rkt")
(provide (contract-out
          (need (any/c . -> . void?))))

(define (need condition)
  (unless condition
    (backtrack)))