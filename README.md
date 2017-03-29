Data and exception analysis for a whole OCaml program

This package contains tools for analysis of OCaml programs.

You can build it using ocp-build and the OCaml compiler version 4.02.3.

In addition of ocp-build, our analyzer uses the Apron library for numerical abstract domains. You can install it with opam as `opam install apron`.

Directories:

 * test: a number of tests that we used during development.
     They are out of date; getting them up to date is still on our work list.
     You can have a look at the `samples` sub-directory, which contains small
     programs that we use to try features of our analyser.

 * src: the sources
  - common: some common functors and types
  - data: the data representation used in a previous version of the analysis
  - driver: an entry point for the analyser, deprecated
  - hgraph: several data structures and algorithms on hypergraphs that
    we use in the analyser
  - lambda: the creation of lambda-code from .cmt and .ml files. (see bytecomp/lambda.mli in your OCaml compiler)
  - alt-compiler-libs: a patched version of the compiler libraries (from 4.02.3) to
    allow more information (types) in our analyser
  - xlambda: the lambda to xlambda translation (see section below)
  - analysis: bridge between the xlambda representation and the data structures
    used for the actual analysis (hgraphs)
  - ocp-analyzer: the main entry point
  - other directories contain experiments either in progress or abandoned

The Xlambda:

Xlambda is an intermediate representation created specially for the purpose of analysis.
The main differences with Lambda:
- Every expression is encapsulated in a let-binding (or a let rec) and at a top-most level ("let x = let y = 1 in y" becomes "let y = 1 in let x = y")
- Functions always take one argument
- Also there are no global variables: they are contained in the functions (with the exception of built-in exceptions)
- "Raise" is not a primitive but an expression
- There are no global values
- && and || have been replaced by if statements
- Arguments (for apply, for, if, primitives) are evaluated before the calls and passed to it as identifiers.
- Primitives cannot raise exceptions
- Identifiers contain the modulename they are defined in.

The function provided in Mk_xlambda should make a valid xlambda representation out of a alt-compiler-libs Lambda.lambda tree.


Using the analyzer:

Our analyzer is intended for analysis of whole OCaml programs. However, its input is either source files or cmt files (generated when compiling with the -bin-annot flag), which must be passed manually on the command-line, in the right order. We have plans to integrate with an other tool, ocp-watcher, that would allow us to recover the right cmt files automatically from an executable, but it is not yet ready.

As an example, to use the analyzer on one of the sample programs in the test/samples directory :

- Build the analyzer with `ocp-build ocp-analyzer`
- Enter the samples directory: `cd test/samples`
- Compile the programs you want to analyze.
  If the program is self-contained, like `loop_no_rec.ml`, use `ocamlc -bin-annot -c -nopervasives loop_no_rec.ml`.
  If the program depends on pervasives, like `a.ml`, you need the corresponding cmt files too. You can use the ones on your system if they are installed, but if they are not the samples directory contains a copy of camlinternalFormatBasics.ml and pervasives.ml that you should compile first: `ocamlc -bin-annot -c -nopervasives camlinternalFormatBasics.ml pervasives.ml`. Then compile your program : `ocamlc -bin-annot -c -nostdlib a.ml`.
- Run the analyzer on the cmt files : `../../_obuild/ocp-analyzer/ocp-analyzer camlinternalFormatBasics.cmt pervasives.cmt a.cmt` (you can copy or link the analyzer executable for convenience).
- In addition to the results displayed on its standard output, the analyzer produces two files `analyzer_output.xlml` and `analyzer_output.dot`. These contain respectively a dump of the xlambda representation of the program, and a dump in dot format of the state of the analysis. They are not meant to be read by end users, but we use them to understand what happens during the analysis. Please include them if you file a bug report.

Tests:

There are a number of tests that were meant to be run with `ocp-build -test`.
Unfortunately, most of them are meant to test code that is not used anymore, and as a result have not been kept up to date. They are currently disabled.

However, the directory test/samples contains a Makefile that can be used to run the analyzer on the sample files. No check is done on the result, but it is used to verify that the analyzer doesn't crash on unexpected language features.


