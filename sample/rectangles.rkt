#lang racket
(require
  jen
  pict)

(define-rule start
  (~>
   (colorize (my-rectangle) (color))
   (colorize (my-rectangle) (color))
   (colorize (my-rectangle) (color))
   #:combiner ht-append))

(define-rule color
  (~> (once) "pink")
  (~> (once) "magenta")
  (~> (once) "purple")
  (~> (once) "indigo"))

(define-rule my-rectangle
  (thunk (filled-rectangle (small) (big)))
  (thunk (filled-rectangle (big) (small))))

(define-rule big
  (thunk 50) #:weight 5
  (thunk 100) #:weight 3
  (thunk 200))

(define-rule small
  (thunk 40) #:weight 5
  (thunk 20) #:weight 3
  (thunk 10))

(module+ main
  (start))