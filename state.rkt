#lang racket
(require "semantics.rkt")
(provide (contract-out
          (turn-on (any/c . -> . void))
          (turn-off (any/c . -> . void))
          (on? (any/c . -> . boolean?))
          (off? (any/c . -> . boolean?))
          (needs (any/c . -> . void))
          (needs-not (any/c . -> . void))))

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