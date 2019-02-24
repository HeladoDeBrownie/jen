#lang racket
(provide
  (contract-out
   (struct rule
     ((clauses (listof clause?))))
   (struct clause
     ((thunk (-> any/c))))))

(struct rule (clauses))

(struct clause (thunk))