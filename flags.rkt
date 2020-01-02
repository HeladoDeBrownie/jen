#lang racket
(require
  (for-syntax syntax/parse)
  "base.rkt"
  "preconditions.rkt")
(provide
  (contract-out
   (on (any/c ... . -> . void))
   (off (any/c ... . -> . void))
   (on? (any/c ... . -> . boolean?))
   (off? (any/c ... . -> . boolean?))
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

(define (toggle-off . flags)
  (need (apply on? flags))
  (apply off flags))

(define (toggle-on . flags)
  (need (apply off? flags))
  (apply on flags))

(define-syntax once
  (syntax-parser
    ((_)
     #`(toggle-on '#,(string->uninterned-symbol "once")))))