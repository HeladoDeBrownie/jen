#lang racket
(require
  "private/weighted-set.rkt")
(provide
 (contract-out
  (struct rule
    ((clauses (hash/c (-> any/c) (-> exact-nonnegative-integer?)))))
  (backtrack (-> none/c))
  (struct exn:backtrack
    ((message string?)
     (continuation-marks continuation-mark-set?)))
  (rule-state parameter?)))

(struct rule (clauses)
  #:property prop:procedure
  (λ (this-rule #:default (default-value default-sentinel))
    (define (go)
      (let try-next ((untried-clauses (rule->weighted-set this-rule)))
        (cond
          ((weighted-set-empty? untried-clauses)
           (when (default-value . equal? . default-sentinel)
             (backtrack))
           default-value)
          (else
           (define-values (clause-to-try untried-clauses_)
             (weighted-set-remove-random untried-clauses))
           (with-handlers ((exn:backtrack? (λ (_) (try-next untried-clauses_))))
             (define-values (result new-state)
               (parameterize ((rule-state (rule-state)))
                 (values (clause-to-try) (rule-state))))
             (rule-state new-state)
             result)))))
    (if (rule-state)
        (go)
        (parameterize ((rule-state #hash()))
          (go)))))

(define default-sentinel (gensym))

(define (rule->weighted-set a-rule)
  (weighted-set
   (for/hash (((thunk weight-thunk) (in-hash (rule-clauses a-rule))))
     (values thunk (weight-thunk)))))

(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

(struct exn:backtrack exn ())

(define rule-state (make-parameter #f))