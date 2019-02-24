#lang racket
(require racket/random)
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))))))

(define (evaluate-rule a-rule)
  ((clause-thunk (random-ref (rule-clauses a-rule)))))

(struct rule (clauses)
  #:property prop:procedure evaluate-rule)

(struct clause (thunk))

(module+ main
  (for ((_ (in-range 100)))
    (displayln ((rule (list
                       (clause (λ () "hewwo :3"))
                       (clause (λ () "hoi :3"))))))))