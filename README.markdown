jen is a Racket package providing tools for procedural generation. While it's geared towards text, it can generate Racket values of any type.

# Installation

Currently there is no stable version of this package, and thus breakage is to be expected. To install it anyway, go to `File→Package Manager→Do What I Mean` in DrRacket and paste the following into the `Package Source` field:

    https://github.com/HeladoDeBrownie/jen.git#Unstable

Alternatively, run the following from command line:

    raco pkg install https://github.com/HeladoDeBrownie/jen.git#Unstable

# Documentation

Documentation is provided in the form of Scribble documents, which will be built automatically upon successful install and can then be found from `Help→Racket Documentation` in DrRacket. (The documentation is currently incomplete, but still potentially useful.)

# Samples

Once jen is installed, you can run the programs in the `samples/` subdirectory of this repository. Try modifying `samples/cat-sentences.rkt` and add your own phrases to it!