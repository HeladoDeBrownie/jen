#lang racket
(require
  "private/weighted-set.rkt")
(provide
  (contract-out
   (evaluate-rule (rule-struct? #:default any/c . -> . any/c))
   (struct rule-struct
     ((clauses (hash/c (-> any/c) (-> exact-nonnegative-integer?)))))
   (backtrack (-> none/c))
   (struct exn:backtrack
     ((message string?)
      (continuation-marks continuation-mark-set?)))
   (make-rule-state-parameter (() (any/c) . ->* . parameter?))))

#| Provided Definitions |#

(define (evaluate-rule a-rule-struct #:default (default-value default-sentinel))
  (define (go)
    (let try-next ((untried-clauses (rule->weighted-set a-rule-struct)))
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
        (go))))

(struct rule-struct (clauses)
  #:property prop:procedure evaluate-rule)

(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

(struct exn:backtrack exn ())

(define (make-rule-state-parameter (initial-value #f))
  (define key (gensym))
  (define (guard new-value) (hash-set (rule-state) key new-value))
  (define (wrap actual-value) (hash-ref actual-value key initial-value))
  (make-derived-parameter rule-state guard wrap))

#| Internal Definitions |#

(define rule-state (make-parameter #f))

; This is used by the rule procedure to detect that the user didn't supply a
; default value.
(define default-sentinel (gensym))

; Produces a weighted set representing the given rule's clauses' weights at the
; present moment.
(define (rule->weighted-set a-rule)
  (weighted-set
   (for/hash (((thunk weight-thunk) (in-hash (rule-struct-clauses a-rule))))
     (values thunk (weight-thunk)))))