#lang racket
(require
  racket/random)
(provide (contract-out
          (weighted-set (any/c . -> . weighted-set/c))
          (weighted-set/c contract?)
          (weighted-set-empty? (weighted-set/c . -> . boolean?))
          (weighted-set-remove-random
           (weighted-set/c . -> . (values any/c weighted-set/c)))))

(define weighted-set make-immutable-hash)

(define weighted-set/c
  (hash/c (-> any/c) exact-positive-integer? #:immutable #t))

(define weighted-set-empty? hash-empty?)

(define (weighted-set-remove-random a-weighted-set)
  (define (expand thunk weight)
    (make-list weight thunk))
  (define choice (random-ref (flatten (hash-map a-weighted-set expand))))
  (values choice (hash-remove a-weighted-set choice)))