#lang racket
(require
  racket/random)
(provide (contract-out
          (weighted-set
           (->
            (hash/c any/c exact-nonnegative-integer?)
            weighted-set/c))
          (weighted-set/c contract?)
          (weighted-set-empty? (weighted-set/c . -> . boolean?))
          (weighted-set-total-weight
           (weighted-set/c . -> . exact-nonnegative-integer?))
          (weighted-set-remove-random
           (weighted-set/c . -> . (values any/c weighted-set/c)))))

(define (weighted-set a-hash)
  (for/hash (((value weight) (in-hash a-hash))
             #:unless (weight . = . 0))
    (values value weight)))

(define weighted-set/c
  (hash/c (-> any/c) exact-nonnegative-integer? #:immutable #t))

(define weighted-set-empty? hash-empty?)

(define (weighted-set-total-weight a-weighted-set)
  (apply + (hash-values a-weighted-set)))

(define (weighted-set-remove-random a-weighted-set)
  (define total-weight (weighted-set-total-weight a-weighted-set))
  (define random-partial-sum (random 0 total-weight))
  (let loop
    ((remaining-elements (hash-keys a-weighted-set))
     (sum-so-far 0))
    (define this-element (first remaining-elements))
    (define this-weight (hash-ref a-weighted-set this-element))
    (define new-sum-so-far (+ sum-so-far this-weight))
    (if (new-sum-so-far . > . random-partial-sum)
        (values this-element (hash-remove a-weighted-set this-element))
        (loop (rest remaining-elements) new-sum-so-far))))