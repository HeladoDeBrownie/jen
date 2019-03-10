#lang scribble/manual
@(require
   (for-label racket)
   (for-label jen/base)
   (for-label jen/syntax))

@title{jen}

jen is a Racket library for procedural generation of text and other data.

@table-of-contents[]

@section{Guide}

TODO

@section{Reference}

@subsection{jen/base}

@defmodule[jen/base]{
 @racketmodname[jen/base] provides the low-level semantics and tools for working
 with rules and clauses.
}

@defstruct*[rule
            ((clauses (hash/c (-> any/c) (-> exact-nonnegative-integer?))))]{
 TODO

 Calling a @racket[rule] value @emph{evaluates} the rule. TODO
}

@defproc[(backtrack) none/c]{
 Signals a backtrack by raising an @racket[exn:backtrack] value.
}

@defstruct*[(exn:backtrack exn) ()]{
 @racket[exn:backtrack] represents the backtrack signal.

 Although usually unnecessary (see documentation for @racket[rule]), it's
 possible to catch a backtrack manually by installing an @racket[exn:backtrack?]
 handler. For example:

 @racketblock[(with-handlers ((exn:backtrack? my-backtrack-handler))
                (my-rule))]
}

@subsection{jen/tag}

@defmodule[jen/tag]{TODO}

@subsection{jen/syntax}

@defmodule[jen/syntax]{
 @racket[jen/syntax] provides clean syntax on top of the rule primitives to make
 it easier to work with them in common cases.
}

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

 The @racket[void?]-filtering behavior is particularly useful alongside
 @racket[jen/tag], which contains many procedures that affect clause evaluation
 and return @racket[(void)].
}