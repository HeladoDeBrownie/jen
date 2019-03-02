#lang racket
(require
  "private/weighted-set.rkt")
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))
      (weight exact-nonnegative-integer?)))
   (struct exn:backtrack
     ((message string?)
      (continuation-marks continuation-mark-set?)))
   (backtrack (-> void))))

(define (evaluate-rule a-rule)
  (let loop ((untried-clauses (rule->weighted-set a-rule)))
    (when (weighted-set-empty? untried-clauses)
      (backtrack))
    (define-values (clause-to-try untried-clauses_)
      (weighted-set-remove-random untried-clauses))
    (with-handlers ((exn:backtrack? (λ (_) (loop untried-clauses_))))
      (clause-to-try))))

(define (rule->weighted-set a-rule)
  (weighted-set
   (map
    (λ (a-clause)
      (cons a-clause (clause-weight a-clause)))
    (rule-clauses a-rule))))

(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

(struct exn:backtrack exn ())

(struct rule (clauses)
  #:property prop:procedure evaluate-rule)

(struct clause (thunk weight)
  #:property prop:procedure (struct-field-index thunk))