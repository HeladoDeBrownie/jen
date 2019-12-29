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

@defstruct*[rule-struct
            ((clauses (hash/c (-> any/c) (-> exact-nonnegative-integer?))))]{
 The @racket[rule-struct] structure represents procedural generation rules, the
 central concept of this library.

 @racket[clauses] is a hash where each mapping represents a clause that may be
 randomly chosen. The key is the @emph{try thunk}, a nullary procedure that's
 applied when the clause is chosen; and the value is the @emph{weight thunk},
 also a nullary procedure, one that's applied to determine the clause's
 likelihood of being chosen.

 Applying a @racket[rule-struct] value is the same as applying
 @racket[evaluate-rule] to it.
}

@defproc[(evaluate-rule (a-rule-struct rule-struct?)
                        (#:default default-value any/c
                         #,(emph "an opaque value"))) any/c]{
 Evaluates a rule as follows:

 The rule's clauses' weight thunks are all evaluated, then try thunks are
 evaluated at random, weighted by their corresponding computed weight, until one
 of them succeeds, i.e., doesn't backtrack. (See @racket[backtrack] and
 @racket[exn:backtrack].) If a clause succeeds, @racket[evaluate-rule] returns
 the result of its try thunk. If @emph{no} clause succeeds, then the entire rule
 backtracks.

 The probability of a clause being chosen is its computed weight divided by the
 total of all clauses' computed weights.

 If a @racket[#:default] argument is provided, it will be the return value in
 case the rule would have otherwise backtracked. This is useful for keeping the
 backtrack signal from bubbling to the top when a rule is being evaluated
 outside of any other rule. (See @racket[exn:backtrack] for how to deal with it
 in general.)
}

@defproc[(backtrack) none/c]{
 Signals a backtrack by raising an @racket[exn:backtrack] value.
}

@defstruct*[(exn:backtrack exn) ()]{
 @racket[exn:backtrack] represents the backtrack signal.

 Although usually unnecessary (see @racket[evaluate-rule]), it's possible to
 catch a backtrack manually by installing an @racket[exn:backtrack?] handler.
 For example:

 @racketblock[(define-rule start)
              
              (with-handlers ((exn:backtrack? (const "It backtracked!")))
                (start))]
}

@racketresult["It backtracked!"]

@defproc[(make-rule-parameter (initial-value any/c #f)) parameter?]{
 Makes a @emph{rule parameter}, which, whenever a clause backtracks, reverts its
 value to whatever it was just before the clause was tried.

 This is useful for tagging rules with additional state, and in fact is used by
 @racketmodname[jen/tag].
}

@section{Rule Syntax}

@defmodule[jen/syntax]{
 @racket[jen/syntax] provides clean syntax on top of the rule primitives to make
 it easier to work with them in common cases.
}

@defform[(rule clause ...)
         #:grammar
         ((clause (code:line proc-expr maybe-weight))
          (maybe-weight (code:line)
                        (code:line #:weight weight-expr)))]{
 Produces a rule whose clauses are given by @racket[clause]s.

 @racket[proc-expr] is taken as the procedure evaluated when trying the clause,
 and @racket[weight-expr] (by default, @racket[1]) is the clause's weight.
}

@defform[(define-rule id rest ...)]{
 Shorthand for @racket[(define id (rule rest ...))].
}

@defform[(~> expr ... maybe-combiner)
         #:grammar
         ((maybe-combiner (code:line)
                          (code:line #:combiner combiner-expr)))]{
 Returns a procedure taking no arguments and returning the result of applying
 @racket[combiner-expr] (by default, @racket[~a]) to each @racket[expr], but
 filtering out any values satisfying @racket[void?]. This is meant for use with
 @racket[rule] or @racket[define-rule] but produces ordinary procedures, much
 like @racket[thunk] but slightly more specialized.

 The @racket[void?]-filtering behavior is particularly useful alongside
 @racketmodname[jen/tag], which contains many procedures that affect clause
 evaluation and return @racket[(void)]. If this turns out to be undesirable,
 it's still possible to use @racket[thunk] with @racket[define-rule].
}

@section{Tagging State}

@defmodule[jen/tag]{TODO}