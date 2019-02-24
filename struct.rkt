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
      (weight positive-integer?)))))

(define (evaluate-rule a-rule)
  (let loop
    ((remaining
      (make-immutable-hash
       (map
        (λ (a-clause)
          (cons (clause-thunk a-clause) (clause-weight a-clause)))
        (rule-clauses a-rule)))))
    (cond
      ((hash-empty? remaining)
       (backtrack))
      (else
       (define-values (thunk now-remaining)
         (choose-randomly-from-weighted-set remaining))
       (with-handlers ((exn:backtrack? (λ (_) (loop now-remaining))))
         (thunk))))))

(define (choose-randomly-from-weighted-set a-weighted-set)
  (define (expand thunk weight)
    (make-list weight thunk))
  (define choice (random-ref (flatten (hash-map a-weighted-set expand))))
  (values choice (hash-remove a-weighted-set choice)))

(define (backtrack)
  (raise (exn:backtrack "backtrack" (current-continuation-marks))))

(struct exn:backtrack exn ())

(struct rule (clauses)
  #:property prop:procedure evaluate-rule)

(struct clause (thunk weight))

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