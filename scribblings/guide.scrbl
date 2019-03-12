#lang scribble/manual
@require[(for-label racket)
         (for-label jen)]

@title{The jen Guide}

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