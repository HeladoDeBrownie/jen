#lang racket
(require
  racket/hash
  racket/random)
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))
      (weight exact-positive-integer?)))
   (backtrack (-> void))
   (rule-state-flags parameter?)))

(define (evaluate-rule a-rule)
  (define (go)
    (let loop
      ((remaining
        (make-immutable-hash
         (map
          (λ (a-clause)
            (cons a-clause (clause-weight a-clause)))
          (rule-clauses a-rule)))))
      (cond
        ((hash-empty? remaining)
         (backtrack))
        (else
         (define-values (a-clause now-remaining)
           (choose-randomly-from-weighted-set remaining))
         (with-handlers ((exn:backtrack? (λ (_) (loop now-remaining))))
           (a-clause))))))
  (if (rule-state-flags)
      (go)
      (parameterize ((rule-state-flags (set)))
        (go))))

(define (choose-randomly-from-weighted-set a-weighted-set)
  (define (expand thunk weight)
    (make-list weight thunk))
  (define choice (random-ref (flatten (hash-map a-weighted-set expand))))
  (values choice (hash-remove a-weighted-set choice)))

(define rule-state-flags (make-parameter #f))

(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

(struct exn:backtrack exn ())

(struct rule (clauses)
  #:property prop:procedure evaluate-rule)

(struct clause (thunk weight)
  #:property prop:procedure (struct-field-index thunk))

(module+ main
  (define start
    (rule (list
           (clause (λ () (~a (greeting) " :3")) 1))))

  (define greeting
    (rule (list
           (clause (λ () "hewwo") 9)
           (clause (λ () "hoi") 1)
           (clause (λ () (~a "this clause always backtracks" (empty))) 10000))))

  (define empty
    (rule (list)))

  (for ((_ (in-range 100)))
    (displayln (start))))