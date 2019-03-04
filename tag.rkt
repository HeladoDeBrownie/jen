#lang racket
(require
  (for-syntax syntax/parse)
  "semantics.rkt")
(provide
  (contract-out
   (on (any/c ... . -> . void))
   (off (any/c ... . -> . void))
   (on? (any/c ... . -> . boolean?))
   (off? (any/c ... . -> . boolean?))
   (need (any/c ... . -> . void))
   (need-not (any/c ... . -> . void))
   (toggle-off (any/c ... . -> . void))
   (toggle-on (any/c ... . -> . void)))
  once)

(define flags-key (string->uninterned-symbol "flags"))
(rule-state (hash-set (rule-state) flags-key (set)))

(define (rule-state-flags (new-value #f))
  (if new-value
      (rule-state (hash-set (rule-state) flags-key new-value))
      (hash-ref (rule-state) flags-key)))

(define (on . flags)
  (rule-state-flags (set-union (rule-state-flags) (list->set flags))))

(define (off . flags)
  (rule-state-flags (set-subtract (rule-state-flags) (list->set flags))))

(define (on? . flags)
  (subset? (list->set flags) (rule-state-flags)))

(define (off? . flags)
  (set-empty? (set-intersect (rule-state-flags) (list->set flags))))

(define (need . flags)
  (unless (apply on? flags)
    (backtrack)))

(define (need-not . flags)
  (unless (apply off? flags)
    (backtrack)))

(define (toggle-off . flags)
  (apply need flags)
  (apply off flags))

(define (toggle-on . flags)
  (apply need-not flags)
  (apply on flags))

(define-syntax (once a-syntax)
  (syntax-parse a-syntax
    ((_)
     #`(toggle-on '#,(gensym)))))