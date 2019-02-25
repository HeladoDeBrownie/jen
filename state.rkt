#lang racket
(require
  (for-syntax syntax/parse)
  "semantics.rkt")
(provide
  (contract-out
   (turn-on (any/c ... . -> . void))
   (turn-off (any/c ... . -> . void))
   (on? (any/c ... . -> . boolean?))
   (off? (any/c ... . -> . boolean?))
   (needs (any/c ... . -> . void))
   (needs-not (any/c ... . -> . void))
   (toggles-off (any/c ... . -> . void))
   (toggles-on (any/c ... . -> . void)))
  once)

(define (turn-on . flags)
  (rule-state-flags (set-union (rule-state-flags) (list->set flags))))

(define (turn-off . flags)
  (rule-state-flags (set-subtract (rule-state-flags) (list->set flags))))

(define (on? . flags)
  (subset? (list->set flags) (rule-state-flags)))

(define (off? . flags)
  (empty? (set-intersect (rule-state-flags) (list->set flags))))

(define (needs . flags)
  (unless (apply on? flags)
    (backtrack)))

(define (needs-not . flags)
  (unless (apply off? flags)
    (backtrack)))

(define (toggles-off . flags)
  (apply needs flags)
  (apply turn-off flags))

(define (toggles-on . flags)
  (apply needs-not flags)
  (apply turn-on flags))

(define-syntax (once a-syntax)
  (syntax-parse a-syntax
    ((_)
     #`(toggles-on '#,(gensym)))))