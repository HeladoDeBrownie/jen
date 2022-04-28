#lang racket
(provide
 (contract-out
  (evaluate-rule (-> rule-struct? #:default any/c any/c))
  (struct rule-struct
    ((clauses (hash/c (-> any/c) (-> weight?)))))
  (backtrack (->* () (string?) none/c))
  (struct exn:backtrack
    ((message string?)
     (continuation-marks continuation-mark-set?)))
  (make-rule-parameter (->* () (any/c) parameter?))
  (weight? predicate/c)))

(require
  "private/weighted-set.rkt")

#| Provided Definitions |#

(define (evaluate-rule a-rule-struct #:default (default-value not-specified))
  (define (go)
    (let try-next ([untried-clauses (rule->weighted-set a-rule-struct)])
      (cond
        [(weighted-set-empty? untried-clauses)
         (when (equal? default-value not-specified)
           (backtrack "all clauses failed"))
         default-value]
        [else
         (define-values (clause-to-try untried-clauses_)
           (weighted-set-remove-random untried-clauses))
         (with-handlers ([exn:backtrack? (λ (_) (try-next untried-clauses_))])
           (define-values (result new-state)
             (parameterize ([rule-state (rule-state)])
               (values (clause-to-try) (rule-state))))
           (rule-state new-state)
           result)])))
  (if (rule-state)
      (go)
      (parameterize ([rule-state #hash()])
        (go))))

(struct rule-struct (clauses)
  #:property prop:procedure evaluate-rule)

(define (backtrack (message #f))
  (raise (exn:backtrack
          (if message
              (string-append "backtrack: " message)
              "backtrack")
          (current-continuation-marks))))

(struct exn:backtrack exn ())

(define (make-rule-parameter (initial-value #f))
  (define key (gensym))
  (define (guard new-value)
    (if (rule-state)
        (hash-set (rule-state) key new-value)
        (raise-outside-rule-error)))
  (define (wrap actual-value)
    (if actual-value
        (hash-ref actual-value key initial-value)
        (raise-outside-rule-error)))
  (make-derived-parameter rule-state guard wrap))

#| Internal Definitions |#

(define rule-state (make-parameter #f))

; This is used by the rule procedure to detect that the user didn't supply a
; default value.
(define not-specified (gensym))

; Produces a weighted set representing the given rule's clauses' weights at the
; present moment.
(define (rule->weighted-set a-rule)
  (weighted-set
   (for/hash ([(thunk weight-thunk) (in-hash (rule-struct-clauses a-rule))])
     (values thunk (weight-thunk)))))

(define (raise-outside-rule-error)
  (raise (exn:fail:contract
          "rule parameter cannot be used outside of rule"
          (current-continuation-marks))))