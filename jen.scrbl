#lang scribble/manual
@(require
   (for-label jen)
   (for-label racket))

@title{jen}

jen is a Racket library for procedural generation of text and other data.

@table-of-contents[]

@defmodule[jen/syntax]{
 @defform[(define-rule id clause ...)
          #:grammar
          ((clause (code:line proc-expr maybe-weight))
           (maybe-weight (code:line)
                         (code:line #:weight weight-expr)))]{
  Binds @racket[id] to a rule whose clauses are given by @racket[clause]s.

  @racket[proc-expr] is taken as the procedure evaluated when trying the clause,
  and @racket[weight-expr] (by default, @racket[1]) is the clause's weight.
 }

 @defform[(~> expr ... maybe-combiner)
          #:grammar
          ((maybe-combiner (code:line)
                           (code:line #:combiner combiner-expr)))]{
  Returns a procedure taking no arguments and returning the result of applying
  @racket[combiner-expr] (by default, @racket[~a]) to each @racket[expr], but
  filtering out any values satisfying @racket[void?].
 }
}