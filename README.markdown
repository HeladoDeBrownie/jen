# jen

jen is a Racket package providing tools for procedural generation. While it's geared towards text, it can generate Racket values of any type.

## Installation

Currently, there is no stable version of this library. Expect the API to change and things to break. To install it anyway, install `jen` from the Racket Package Index, which also installs the following packages:

- `jen-lib`, which provides the actual library
- `jen-doc`, which documents the library
- `jen-samples`, which includes runnable samples

Installation can be done through one of the following methods:

### DrRacket

1. Open DrRacket.
2. Go to `File→Package Manager...→Available from Catalog`.
3. Type `jen` into the search.
4. Select the package named `jen`.
5. Press `Install`.

### Command Line

Open a terminal and run the following command:

    raco pkg install jen

Press `a` followed by enter to confirm that you wish to install all of its dependencies.

## Documentation

Documentation is provided in the form of Scribble documents, which will be built automatically upon successful installation of the `jen-doc` package and can then be found from `Help→Racket Documentation` in DrRacket. The guide portion of the documentation has mostly yet to be written, but the reference covers the entire API.
