#lang racket
(require racket/random)
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))
      (weight positive-integer?)))))

(define (evaluate-rule a-rule)
  (define (expand a-clause)
    (make-list (clause-weight a-clause) (clause-thunk a-clause)))
  (define expanded-clauses (flatten (map expand (rule-clauses a-rule))))
  (let loop ((remaining (shuffle expanded-clauses)))
    (match remaining
      ((cons thunk now-remaining)
       (with-handlers ((exn:backtrack? (λ (_) (loop now-remaining))))
         (thunk)))
      (_
       (backtrack)))))

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
           (clause (λ () (~a "this clause always backtracks" (empty))) 1))))
  (define empty
    (rule (list)))
  (for ((_ (in-range 100)))
    (displayln (start))))