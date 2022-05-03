#lang racket
(provide
 (contract-out
  (struct rule-struct
    ((clauses (listof (cons/c (-> any/c) (-> weight?))))))
  (weight? predicate/c)
  (rule-evaluate (-> rule-struct? #:default any/c any/c))
  (backtrack (->* () (string?) none/c))
  (struct exn:backtrack
    ((message string?)
     (continuation-marks continuation-mark-set?)))
  (make-rule-parameter (->* () (any/c) parameter?))
  (current-clause-selector (parameter/c clause-selector/c))
  (default-clause-selector clause-selector/c)
  (clause-selector/c contract?)))

(define weight?
  (and/c rational? (not/c negative?) exact?))

(define (rule-evaluate a-rule-struct #:default (default-value not-specified))
  ; Try clauses at random until one succeeds (i.e., doesn't backtrack) or there
  ; are none left.
  (define (try-next (remaining-clauses
                     (evaluate-weights (rule-struct-clauses a-rule-struct))))
    (cond
      ; There are no clauses left.
      [(empty? remaining-clauses)
       (if (equal? default-value not-specified)
           (backtrack "all clauses failed")
           default-value)]
      ; There is at least one clause left.
      [else
       (define-values (selected-clause remaining-clauses_)
         (select-clause remaining-clauses))
       (with-handlers*
           ; If this clause backtracks, try another.
           ([exn:backtrack? (thunk* (try-next remaining-clauses_))])
         (define-values (result new-rule-state)
           (parameterize ([rule-state (rule-state)])
             ; Try the clause.
             (values (selected-clause) (rule-state))))
         ; The clause succeeded; propagate the new rule state and return.
         (rule-state new-rule-state)
         result)]))
  ; Install a fresh rule state if there isn't one already. This normally
  ; happens when a rule is evaluated outside of any other rule.
  (if (rule-state)
      (try-next)
      (parameterize ([rule-state #hash()])
        (try-next))))

(struct rule-struct (clauses)
  #:property prop:procedure rule-evaluate)

(define (backtrack (message #f))
  (raise (exn:backtrack
          (if message
              (string-append-immutable "backtrack: " message)
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

(define (default-clause-selector total-weight _)
  (random 0 total-weight))

(define current-clause-selector (make-parameter default-clause-selector))

(define clause-selector/c
  (->i ([total-weight natural?]
        [_ (listof (cons/c (-> any/c) natural?))])
       [_ (total-weight) (and/c natural? (</c total-weight))]))

#| Private Definitions |#

; A sentinel used to determine whether rule-evaluate was given a default value.
(define not-specified (string->uninterned-symbol "not specified"))

; Turn the weight thunks of the given clauses into natural numbers by
; evaluating them and multiplying them all by the same amount.
(define (evaluate-weights clauses)
  (define (map-cdrs f a-dict)
    (dict-map a-dict (λ (key value) (cons key (f value)))))
  (define unnormalized-clauses
    (map-cdrs (λ (weight-thunk) (weight-thunk)) clauses))
  (define divisor
    (apply gcd (dict-values unnormalized-clauses)))
  (map-cdrs (λ (x) (/ x divisor)) unnormalized-clauses))

(define (select-clause clauses)
  (define total-weight (apply + (dict-values clauses)))
  (define target-weight ((current-clause-selector) total-weight clauses))
  (let loop
    ([remaining-clauses clauses]
     (sum-so-far 0))
    (match-define (cons try-thunk weight) (first remaining-clauses))
    (define new-sum-so-far (+ sum-so-far weight))
    (if (> new-sum-so-far target-weight)
        (values try-thunk (dict-remove clauses try-thunk))
        (loop (rest remaining-clauses) new-sum-so-far))))

(define rule-state (make-parameter #f))

(define (raise-outside-rule-error)
  (raise (exn:fail:contract
          "rule parameter cannot be used outside of rule"
          (current-continuation-marks))))