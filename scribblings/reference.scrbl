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

 Applying a @racket[rule] value is the same as applying @racket[evaluate-rule]
 to it.
}

@defproc[(evaluate-rule (a-rule rule?) (#:default default-value any/c
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

 Rule evaluation is parameterized by @racket[rule-state], which will normally be
 a hash as long as a rule is being evaluated. This hash isn't read or written to
 by rule evaluation itself, but can be used by other modules that wish to make
 some state backtrackable. Whenever a rule evaluated in the course of another
 rule's evaluation modifies this state, if that subrule ends up backtracking,
 the changes it made will be reverted.

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

@defparam[rule-state state (or/c hash? #f) #:value #f]{
 @racket[rule-state] is used for @racket[rule]'s backtrackable state.

 It's usually safe to read and write a specific key from the rule state hash,
 but modifying it in other ways may cause modules relying on it to misbehave,
 and is therefore @emph{unsafe}.
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