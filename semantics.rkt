#lang racket
(require
  "weighted-set.rkt")
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))
      (weight exact-positive-integer?)))
   (backtrack (-> void))))

(define (evaluate-rule a-rule)
  (let loop ((remaining (rule->weighted-set a-rule)))
    (cond
      ((hash-empty? remaining)
       (backtrack))
      (else
       (define-values (a-clause now-remaining)
         (weighted-set-remove-random remaining))
       (with-handlers ((exn:backtrack? (λ (_) (loop now-remaining))))
         (a-clause))))))

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