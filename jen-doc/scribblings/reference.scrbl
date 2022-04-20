#lang scribble/manual
@require[(for-label racket)
         (for-label jen)]

@title{The jen Reference}

@defmodule[jen]{
 @racketmodname[jen] reprovides all of the modules in this package.
}

@section{Semantics}

@defmodule[jen/base]{
 @racketmodname[jen/base] provides the low-level semantics and tools for working
 with rules and clauses.
}

@defstruct*[rule-struct
            ((clauses (hash/c (-> any/c) (-> weight?))))]{
 The @racket[rule-struct] structure represents procedural generation rules, the
 central concept of this library.

 @racket[clauses] is a hash where each mapping represents a clause that may be
 randomly chosen. The key is the @emph{try thunk}, a nullary procedure that's
 applied when the clause is chosen; and the value is the @emph{weight thunk},
 also a nullary procedure, one that's applied to determine the clause's
 likelihood of being chosen (see @racket[weight?]).

 Applying a @racket[rule-struct] value is the same as applying
 @racket[evaluate-rule] to it.
}

@defproc[(weight? (a-value any/c)) boolean?]{
 Recognizes weights, which are numbers used to describe the relative likelihood
 of a clause being selected. A weight is @racket[rational?] (including
 integers), non-@racket[negative?], and @racket[exact?].
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

@defproc[(backtrack (message string? #f)) none/c]{
 Signals a backtrack by raising an @racket[exn:backtrack] value.

 If @racket[message] is provided, it will be used to provide a more descriptive
 error message for debugging purposes.
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

 This is useful for tagging rules with additional state that needs to remain
 consistent with respect to clauses being tried but failing.
}

@section{Syntax}

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

 For each clause, @racket[proc-expr] is evaluated immediately to obtain the
 clause's try thunk, while @racket[weight-expr] (by default, @racket[1]) becomes
 the body of the weight thunk and thus is evaluated only when the rule is.
}

@defform[(define-rule id rest ...)]{
 Shorthand for @racket[(define id (rule rest ...))].
}

@defform[(simple-rule expr ...)]{
 Shorthand for @racket[(rule (thunk expr) ...)].

 This is useful when each of your clauses only needs to be a single expression
 without complex logic, allowing you to omit @racket[~>] or other
 procedure-producing forms.
}

@defform[(define-simple-rule id rest ...)]{
 Shorthand for @racket[(define id (simple-rule rest ...))].
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
 @racketmodname[jen/preconditions], which contains procedures and forms that
 affect clause evaluation and return @racket[(void)]. If this turns out to be
 undesirable for a given use case, it's still possible to use @racket[thunk]
 with @racket[define-rule].
}

@section{Preconditions}

@defmodule[jen/preconditions]{
 @racket[jen/preconditions] provides support for clause preconditions, by which
 a clause can decide not to execute if its conditions aren't met.
}

@defproc[(need (condition any/c)) void?]{
 Returns a @racket[void?] value when @racket[condition] is true; otherwise
 signals a backtrack by calling @racket[(backtrack)].
}

@defform[(once)]{
 Shorthand for @racket[(n-times 1)].
}

@defform[(n-times n)
         #:contracts ((n natural?))]{
 During the current top-level rule evaluation, if the clause containing the call
 site of this form has previously been committed to less than @racket[n] times,
 the form evaluates to a @racket[void?] value. Otherwise, it signals a backtrack
 (see @racket[backtrack]).

 The upshot of this is that the clause containing the call to this macro can be
 allowed to be committed to at most @racket[n] times before it always fails.

 Underlyingly, each call site of this macro uses its own @emph{rule parameter}
 (see @racket[make-rule-parameter]) as a counter of how many times it's been
 reached (and its clause committed to).
}

@section{Sequences}

@defmodule[jen/sequential]{
 @racket[jen/sequential] contains forms for non-random, ordered evaluation that
 nonetheless depends on the current rule state.
}

@defform[(cycle expr ...+)]{
 Cycles through the @racket[expr]s, returning the value of one each time this
 form is evaluated as part of a clause. If the clause has never been committed
 to before, the expression will be the first one; if it has been committed to
 once, it will be the second one; and so on. Once all expressions have been
 exhausted, it begins anew with the first one.
}
