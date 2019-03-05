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

#| Provided Definitions |#

#| This structure represents rules and how to evaluate them. Evaluate a rule by
calling it like a normal procedure. Additionally, calling it with a #:default
argument will cause it to return the specified value instead of backtracking in
case the entire rule fails. |#
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

#| Signal a backtrack by raising an exn:backtrack. |#
(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

#| This structure represents the backtrack signal. In particular, exn:backtrack?
can be used to detect a backtrack. Normally, this isn't necessary in end code,
but since exn:backtrack can escape this module, this is provided to handle
it. |#
(struct exn:backtrack exn ())

#| This parameter is used for the rule procedure's backtrackable state. Using it
directly is potentially UNSAFE because there's no interface to mediate the state
installed by different libraries. Until that exists, use this with caution. |#
(define rule-state (make-parameter #f))

#| Internal Definitions |#

; default-sentinel is used by the rule procedure to detect that the user didn't
; supply a default value.
(define default-sentinel (gensym))

; Produce a weighted set representing the given rule's clauses' weights at the
; present moment.
(define (rule->weighted-set a-rule)
  (weighted-set
   (for/hash (((thunk weight-thunk) (in-hash (rule-clauses a-rule))))
     (values thunk (weight-thunk)))))