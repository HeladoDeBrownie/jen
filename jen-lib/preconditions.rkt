#lang racket
(require
  syntax/parse/define
  "base.rkt")
(provide (contract-out
          (need (any/c . -> . void?)))
         once
         n-times)

(define (need condition (message #f))
  (unless condition
    (backtrack
     (if message
         message
         "precondition failed"))))

(define-syntax-rule
  (once)
  (n-times 1))

(define-syntax-parser n-times
  ((_ n)
   #:declare n (expr/c #'natural?)
   (let ((current-count
          (syntax-local-lift-expression #'(make-rule-parameter 0))))
     #`(begin
         (#,current-count (add1 (#,current-count)))
         (need
          (<= (#,current-count) n.c)
          (~a "exceeded maximum number of times committed (" n ")"))))))