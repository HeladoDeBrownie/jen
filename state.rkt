#lang racket
(require
  (for-syntax syntax/parse)
  "semantics.rkt")
(provide
  (contract-out
   (turn-on (any/c . -> . void))
   (turn-off (any/c . -> . void))
   (on? (any/c . -> . boolean?))
   (off? (any/c . -> . boolean?))
   (needs (any/c . -> . void))
   (needs-not (any/c . -> . void))
   (toggles-off (any/c . -> . void))
   (toggles-on (any/c . -> . void)))
  once)

(define (turn-on a-flag)
  (rule-state-flags (set-add (rule-state-flags) a-flag)))

(define (turn-off a-flag)
  (rule-state-flags (set-remove (rule-state-flags) a-flag)))

(define (on? a-flag)
  (set-member? (rule-state-flags) a-flag))

(define (off? a-flag)
  (not (set-member? (rule-state-flags) a-flag)))

(define (needs a-flag)
  (unless (on? a-flag)
    (backtrack)))

(define (needs-not a-flag)
  (unless (off? a-flag)
    (backtrack)))

(define (toggles-off a-flag)
  (needs a-flag)
  (turn-off a-flag))

(define (toggles-on a-flag)
  (needs-not a-flag)
  (turn-on a-flag))

(define-syntax (once a-syntax)
  (define a-flag (gensym))
  (syntax-parse a-syntax
    ((_)
     #`(begin
         (needs-not '#,a-flag)
         (turn-on '#,a-flag)))))