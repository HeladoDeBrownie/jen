jen is a Racket package providing tools for procedural generation. While it's geared towards text, it can generate Racket values of any type.

# Installation

Currently there is no stable version of this package, and thus breakage is to be expected. To install it anyway, install `jen` from the Racket Package Index through one of these methods:

## DrRacket

1. Open DrRacket.
2. Go to `File→Package Manager...→Available from Catalog`.
3. Type `jen` into the search.
4. Select the package named `jen`.
5. Press `Install`.

## Command Line

Open a terminal and run the following command:

    raco pkg install jen

Press `a` followed by enter to confirm that you wish to install all of its dependencies.

# Documentation

Documentation is provided in the form of Scribble documents, which will be built automatically upon successful install of the `jen-doc` package and can then be found from `Help→Racket Documentation` in DrRacket. (The jen Guide has mostly yet to be written, but the jen Reference covers jen's entire API.)

# Samples

Once jen is installed, you can run the programs in the `jen-samples/samples/` subdirectory of this repository. Try modifying `samples/cat-sentences.rkt` and adding your own phrases to it!
