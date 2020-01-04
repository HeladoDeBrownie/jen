#lang info
(define collection  "jen")
(define version     "0.0.0")
(define deps        '("base"))
(define build-deps  '("jen-lib"
                      "racket-doc"
                      "scribble-lib"))
(define pkg-desc    "procedural generation DSL embedded in Racket (doc)")
(define pkg-authors '("helado de brownie"))
(define scribblings '(("scribblings/jen.scrbl" (multi-page) (library))))
