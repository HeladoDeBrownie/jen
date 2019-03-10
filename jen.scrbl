#lang scribble/manual
@(require
   (for-label racket)
   (for-label jen/base)
   (for-label jen/syntax))

@title{The jen Library}

jen is a Racket library for procedural generation of text and other data.

@table-of-contents[]

@section{Guide}

TODO

One of the simplest jen programs that produces output is:

@racketblock[(require jen)

             (define-rule start
               (~> "hello friend :3"))

             (start)]

This always gives @racketresult["hello friend :3"]. But jen is designed to give
@emph{random} results, so it's more interesting to try rules with multiple
alternatives:

@racketblock[(require jen)

             (define-rule start
               (~> "hello friend :3")
               (~> "greetings friend :3"))

             (start)]

Now when you run the program, you'll get either @racketresult["hello friend :3"]
or @racketresult["greetings friend :3"], randomly. Try running it multiple times
to see, or, if you're using an interactive evaluator such as DrRacket, run just
@racket[(start)] over and over.

Some of the text in this rule is fixed, meaning it's the same for all (both)
alternatives. We can avoid writing it more than once by factoring out part of
this rule into a second rule:

@racketblock[(require jen)

             (define-rule start
               (~> (greeting) " friend :3"))

             (define-rule greeting
               (~> "hello")
               (~> "greetings"))

             (start)]

@margin-note{
 There's nothing special about the names of rules. @racketid[greeting] is just
 as much a rule as @racketid[start] is. The only difference is we apply
 @racket[(start)] directly and not @racket[(greeting)]. Try applying
 @racket[(greeting)] yourself and see if it works as you expect.
}

We can even generalize this further and get multiple independently random parts:

@racketblock[(require jen)

             (define-rule start
               (~> (greeting) " " (friend) " :3"))

             (define-rule greeting
               (~> "hello")
               (~> "greetings"))

             (define-rule friend
               (~> "friend")
               (~> "amigx"))

             (start)]

Now, there are @emph{four} possible results, one for every combination of the
two choices that can be made for each of the two independent rules,
@racketid[greeting] and @racketid[friend]. Try running @racket[(start)] until
you see them all.

@margin-note{
 Try adding more possibilities to @racketid[greeting] and @racketid[friend]
 yourself, or factoring @racket[":3"] out and adding more possible emotes.
}

TODO

@section{Reference}

@subsection{jen}

@defmodule[jen]{
 @racketmodname[jen] reprovides all of the modules described hereafter.
}

@subsection{jen/base}

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
 filtering out any values satisfying @racket[void?]. This is meant for use with
 @racket[define-rule] but produces ordinary procedures, much like
 @racket[thunk] but slightly more specialized.

 The @racket[void?]-filtering behavior is particularly useful alongside
 @racketmodname[jen/tag], which contains many procedures that affect clause
 evaluation and return @racket[(void)]. If this turns out to be undesirable,
 it's still possible to use @racket[thunk] with @racket[define-rule].
}