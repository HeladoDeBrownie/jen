#| #aesthetic Generator
   Ported from https://helado.itch.io/aesthetic-generator

   Content warning: This program may generate phrases suggestive of violence or
   of sexual contact. |#
#lang racket
(require jen)

(define-rule origin
  (~> (affixed-phrase)) #:weight 2
  (~> (affixed-word)) #:weight 1)

(define-rule affixed-phrase
  (~> (prefixed-phrase) (maybe-phrase-suffix)))
(define-rule prefixed-phrase
  (~> (phrase-prefixing-word) " " (maybe-affixed-word)))
(define-rule phrase-prefixing-word
  (~> (phrase-prefix))
  (~> (maybe-affixed-word)))
(define-rule maybe-affixed-word
  (~> (affixed-word)) #:weight 2
  (~> (genre)) #:weight 1)
(define-rule maybe-phrase-suffix
  (~>) #:weight 24
  (~> " " (phrase-suffix)) #:weight 1)

(define-rule affixed-word
  (~> (prefixed-word)) #:weight 5
  (~> (suffixed-word)) #:weight 1)
(define-rule prefixed-word
  (~> (word-prefix) (prefixable-word)))
(define-rule prefixable-word
  (~> (genre)))
(define-rule suffixed-word
  (~> (suffixable-word) (word-suffix)))
(define-rule suffixable-word
  (~> (genre)) #:weight 2
  (~> (suffixable-non-genre)) #:weight 1)
(define-rule suffixable-non-genre
  (~> (phrase-prefix))
  (~> (word-prefix)))

(define-simple-rule genre
  "biker"
  "butch"
  "clown"
  "cosplay"
  "cowboy"
  "disco"
  "dragon"
  "emo"
  "fairy"
  "farmer"
  "folk"
  "funk"
  "furry"
  "gamer"
  (geek)
  "glitch"
  "goth"
  "grunge"
  "hacker"
  "hip hop"
  "hippie"
  "hipster"
  "jock"
  "larper"
  "librarian"
  "metal"
  "monster"
  "noir"
  "pirate"
  "prep"
  (princex)
  "punk"
  "raver"
  "ska"
  "skater"
  "taur"
  "teacher"
  "trucker"
  "twink"
  "witch"
  (movement-genre))
(define-simple-rule geek
  "geek"
  "nerd"
  "otaku")
(define-simple-rule princex
  "princex"
  "princess"
  "prince")
(define-simple-rule movement-genre
  "baroque"
  "brutalist"
  "classical"
  "cubist"
  "dadaist"
  "modernist"
  "realist"
  "surrealist")

(define-simple-rule phrase-prefix
  "8-bit"
  "abstract"
  "art"
  "ballroom"
  "beach"
  "bespoke"
  "bubblegum"
  "business"
  "candy"
  "carnival"
  "crypt"
  "dark"
  "death"
  "deep"
  "disaster"
  "doom"
  "experimental"
  "fairytale"
  "fantasy"
  "forest"
  "future"
  "glam"
  "hard"
  "health"
  "hell"
  "horror"
  "industrial"
  "lesbian"
  "magic"
  "math"
  "medical"
  "minimal"
  "myth"
  "nature"
  "neon"
  "nu"
  "operatic"
  "pajama"
  "pasta"
  "pastel"
  "postapocalypse"
  "power"
  "psychedelic"
  "rainbow"
  "sci-fi"
  "soft"
  "spicy"
  "swamp"
  "symphonic"
  (toon)
  (decade-phrase-prefix)
  (geographic-phrase-prefix)
  (proper-phrase-prefix))
(define-simple-rule toon
  "toon"
  "anime")
(define-simple-rule decade-phrase-prefix
  "70s"
  "80s"
  "90s")
(define-simple-rule geographic-phrase-prefix
  "Celtic"
  "Euro"
  "French"
  "Italo"
  "Latin"
  "Martian"
  "New England"
  "Scandinavian"
  "Southern"
  "Texas"
  "Viking")
(define-simple-rule proper-phrase-prefix
  "Disney"
  "Hollywood"
  "Instagram"
  "Neopet"
  "Nintendo")

(define-simple-rule phrase-suffix
  (dx)
  "kei")
(define-simple-rule dx
  "DX"
  "EX")

(define-simple-rule word-prefix
  "acid"
  "astro"
  "bio"
  "care"
  "chill"
  "choco"
  "crypto"
  "cyber"
  "e-"
  "electro"
  "endo"
  "enviro"
  "fem"
  "fuck"
  "fur"
  "glow"
  "gore"
  "holo"
  "infra"
  "jump"
  "litho"
  "macro"
  "mecha"
  "mer"
  "meta"
  "mono"
  "nano"
  "neo"
  "night"
  "norm"
  "post"
  "pre"
  "proto"
  "pseudo"
  "retro"
  "sea"
  "semi"
  "steam"
  "synth"
  "techno"
  "trans"
  "vapor"
  (geographic-word-prefix))
(define-simple-rule geographic-word-prefix
  "Afro"
  "Brit"
  "J-"
  "K-")

(define-simple-rule word-suffix
  "beat"
  "core"
  "dance"
  "fuck"
  "pop"
  "punk"
  "style"
  "wave")

(define (main)
  (displayln (origin #:default "Oops, something went wrong.")))

(module+ main
  (main))