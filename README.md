Files:
* Syntax
    * Parser.hs: the parsing library
    * Expression.hs: the internal expression representation
    * Grammar.hs: the language grammar
* Evaluation
    * Substitution.hs: substitution rules and free variables
    * Normal.hs: rules for small-step normal-order evaluation
    * Applicative.hs: rules for small-step applicative-order evaluation
    * Big.hs: big-step evaluation of single expressions and expression lists
* Typing
    * Type.hs: internal type representation
    * Unification.hs: type unification
    * Inference.hs: type inference
* Main.hs: main module
* prog.txt: sample program

See the tests for use cases.

The tests make the following assumptions:
* The data type that encodes lambda expressions instantiates the 'Show' class.
* The Grammar module exposes the 'parseProgram' function, which takes
  a program string and returns a list of internal expression representations. 
* The Substitution module exposes
    * the 'subst' function, which encodes the substitution rules
    * the 'freeVars' function, which returns the list of free variable names
      within a given expression.
* In case of conflict, the 'subst' function renames the bound variable
  by appending a '#' to its name.

To run the tests:
   * cabal install --lib --package-env . base HTF containers Cabal-syntax mtl
   * runhaskell -isrc -itest test/InterpreterTest.hs
