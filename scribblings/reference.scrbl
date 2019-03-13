#lang scribble/manual
@require[(for-label racket)
         (for-label jen)]

@title{The jen Reference}

@defmodule[jen]{
 @racketmodname[jen] reprovides all of the modules in this package.
}

@section{Rule Semantics}

@defmodule[jen/base]{
 @racketmodname[jen/base] provides the low-level semantics and tools for working
 with rules and clauses.
}

@defstruct*[rule
            ((clauses (hash/c (-> any/c) (-> exact-nonnegative-integer?))))]{
 The @racket[rule] structure represents procedural generation rules, the central
 concept of this library.

 @racket[clauses] is a hash where each mapping represents a clause that may be
 randomly chosen. The key is the @emph{try thunk}, a nullary procedure that's
 applied when the clause is chosen; and the value is the @emph{weight thunk},
 also a nullary procedure, one that's applied to determine the clause's
 likelihood of being chosen.

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

@section{Rule Syntax}

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
 filtering out any values satisfying @racket[void?]. This is meant for use with
 @racket[define-rule] but produces ordinary procedures, much like
 @racket[thunk] but slightly more specialized.

 The @racket[void?]-filtering behavior is particularly useful alongside
 @racketmodname[jen/tag], which contains many procedures that affect clause
 evaluation and return @racket[(void)]. If this turns out to be undesirable,
 it's still possible to use @racket[thunk] with @racket[define-rule].
}

@section{Tagging State}

@defmodule[jen/tag]{TODO}