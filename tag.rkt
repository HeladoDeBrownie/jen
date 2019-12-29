#lang racket
(require
  (for-syntax syntax/parse)
  "base.rkt")
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

(define current-flags (make-rule-parameter (set)))

(define (on . flags)
  (current-flags (set-union (current-flags) (list->set flags))))

(define (off . flags)
  (current-flags (set-subtract (current-flags) (list->set flags))))

(define (on? . flags)
  (subset? (list->set flags) (current-flags)))

(define (off? . flags)
  (set-empty? (set-intersect (current-flags) (list->set flags))))

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