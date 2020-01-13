#lang scribble/manual
@require[(for-label racket
                    jen)]

@title{The jen Guide}

jen is a tool for procedurally generating text and other things. TODO

This guide is aimed at people with relatively little experience in Racket. It's
intended that you try the code in DrRacket as it comes up in the text.

@section{Rules}

A @emph{rule} is a value representing a way to generate something, which is the
central concept in jen. Rules can be defined and applied like so:

@margin-note{
 The examples in this guide all begin the same way:

 @codeblock{
  #lang racket
  (require jen)
 }

 This tells Racket that we're using the standard Racket language and importing
 the jen library. Since this text is necessary for all the examples, it's
 included so you don't need to remember to keep it in.
}

@codeblock{
 #lang racket
 (require jen)

 (define-simple-rule color
   "red"
   "blue")

 (color)
}

When you run this program, you should see either @racket{red} or @racket{blue}
in the interactions pane. (Try running the program again until you've seen
both. You can also enter @racket[(color)] into the interactions pane instead of
running the whole program again.)