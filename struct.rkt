#lang racket
(require
  racket/hash
  racket/random)
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))
      (default-clause (or/c default-clause? #f))))
   (struct clause
     ((thunk (-> any/c))
      (weight positive-integer?)))
   (struct default-clause
     ((thunk (-> any/c))))))

(define (evaluate-rule a-rule)
  (let loop ((remaining
              (make-immutable-hash
               (map
                (λ (a-clause)
                  (cons (clause-thunk a-clause) (clause-weight a-clause)))
                (rule-clauses a-rule)))))
    (cond
      ((hash-empty? remaining)
       (define a-default-clause (rule-default-clause a-rule))
       (if a-default-clause
           ((default-clause-thunk a-default-clause))
           (backtrack)))
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

(struct rule (clauses default-clause)
  #:property prop:procedure evaluate-rule)

(struct clause (thunk weight))

(struct default-clause (thunk))

(module+ main
  (define start
    (rule (list
           (clause (λ () (~a (greeting) " :3")) 1))
          #f))
  (define greeting
    (rule (list
           (clause (λ () "hewwo") 9)
           (clause (λ () "hoi") 1)
           (clause (λ () (~a "this clause always backtracks" (empty))) 10000))
          #f))
  (define empty
    (rule (list) #f))
  (for ((_ (in-range 100)))
    (displayln (start))))