; by helado de brownie and Crossroads Wanderer
#lang racket
(require jen)

(define-rule start
  (~> (on 'fur)
      "You spy " (fur-description) " " (cat) " with " (eye-color) " eyes. "
      "It's playing with " (a-toy) ". When it sees you, " (it-does-something)
      "." (whose-cat-is-this)))

(define-rule fur-description
  (~> (fur-length) (maybe-fur-color)))

(define-rule maybe-fur-color
  (~> (need 'fur) " " (fur-color))
  (~> (need-not 'fur)))

(define-rule fur-length
  (~> "a short-haired") #:weight 10
  (~> "a long-haired") #:weight 9
  (~> "a mangey") #:weight 2
  (~> (off 'fur) "a furless") #:weight 1)

(define-rule fur-color
  (~> "orange")
  (~> "black")
  (~> "white")
  (~> "gray")
  (~> "calico"))

(define-rule cat
  (~> "cat") #:weight 10
  (~> "kitty") #:weight 10
  (~> (on 'kitten) "kitten") #:weight 5
  (~> "creature") #:weight 2
  (~> "goblin") #:weight 2
  (~> "gremlin") #:weight 2
  (~> "pillow with knives"))

(define-rule eye-color
  (~> "green")
  (~> "yellow")
  (~> "orange")
  (~> "hazel")
  (~> "blue"))

(define-rule a-toy
  (~> "a rubber ball") #:weight 2
  (~> "a toy mouse") #:weight 2
  (~> "a feather toy") #:weight 2
  (~> "a toy bird") #:weight 2
  (~> "a hairbrush") #:weight 2
  (~> "a wet clump of hair"))

(define-rule it-does-something
  (~> "it scales your leg with its claws")
  (~> "it " (meows) " " (at-you))
  (~> "it rubs up against your leg")
  (~> "it rears up to pet itself with your hand" (maybe-falls-over))
  (~> "it wails at you in a language lost to time")
  (~> "it gets its claws caught in the carpet and falls over")
  (~> "it leaps five feet into the air and scrabbles away in a panic"))

(define-rule maybe-falls-over
  (~>) #:weight (if (on? 'kitten) 1 9)
  (~> ", but falls over instead") #:weight (if (on? 'kitten) 9 1))

(define-rule meows
  (~> "meows piteously")
  (~> "meows forlornly")
  (~> "meows grumpily")
  (~> "meows cheerfully")
  (~> "chirps")
  (~> "makes grumpy noises"))

(define-rule at-you
  (~> "at you") #:weight 4
  (~> "in your direction")
  (~> "at no one in particular"))

(define-rule whose-cat-is-this
  (~>) #:weight 19
  (~> " This is not your cat."))

(define (main)
  (displayln (start)))

(module+ main
  (main))