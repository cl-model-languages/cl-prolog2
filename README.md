
# Cl-Prolog2 - Common Interface to the ISO Prolog implementations from Common Lisp

This is a realization of Marc Kuo's ["modelling approach to OR (operations research)"](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/)
for Prolog language.

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke State-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.
Choose the one with the *best performance for your application*.

**News** We changed the implementation to use the batch-mode only, since the behavior of Prolog top-level loop
is not defined in the ISO standard, and it is hard to maintain the compatibility between different interpreters.

For a practical guide to write a fast, efficient prolog, [this page](https://www.metalevel.at/prolog/efficiency) might help.

## Related work

It looks like https://github.com/keithj/cl-prolog has a similar idea in mind.
However, it is based on a FFI binding written for each interpreter,
thus it does not scale to a larger number of prolog systems.

There are already many lisp-to-prolog libraries, including the one mentioned above, but
also Allegro Prolog, PAIP prolog and the Prolog in On Lisp are the famous ones.
They rather implemented a Prolog system by itself, i.e., [programming OR approach](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/).
Although these are different approaches,
it is quite possible to support those systems from cl-prolog2.

## Supported implementations

* SWI-prolog, the most actively developed prolog system.
* YAP, another well-maintained prolog system.
* XSB, famous for introducing tabling semantics.
* B-Prolog, a commercial implementation by Afany Software, which is free-to-use
  for academic purpose. Claiming to be fast, but the
  [benchmark](http://www.picat-lang.org/bprolog/performance.htm) was taken many
  years ago.

## API

The ASDF system `cl-prolog2` does not provide implementations, but merely the API to those implementations.
The sub-libraries of `cl-prolog2` are in the corresponding sub-directories.

They should implement a method `(run-prolog rules prolog-designator &key debug args &allow-other-keys)`,
where `prolog-designator` is a keyword symbol such as `:swi` or `:yap`.
The function returns the output of the process as a string.

* `:debug` --- when non-nil, print the prolog output to the standard output.
* `:args` --- provides additional command line argument to the prolog
  interpreter. For example, `:args '("-g" "main")` for swi-prolog says that the top-level goal is `main`.

**We don't provide a parser for Prolog output** and, therefore,
**formatting the output should be done by Prolog** or you should **write a parser from lisp**.
Also, consider using `print-term-sexp` below. 

## Query format

    rule : (:- top-term top-term*) | top-term
    
    top-term : ('not term) | ( atom term* )
    
    term : builtins | ( atom term* ) | atom | variable | number | string
    
    variable : symbols starting with ? or _

    atom : other symbols
    
    builtins : ('list term*) | ('list* term*) | ('not term) | ('or term*) | ('and term*)
    
Predicates and atoms are wrapped in single quotes.

Variables are given to the Prolog interpreter as underscored symbols, and
note that **all non-alphanumeric characters are converted to underscores**.
This is more than enough for most use cases because variables are converted to gensyms by the prolog interpreter anyways,
and we cannot expect the variable names are maintained.
Care must be taken when two variables ends up in the same name, e.g. both `?a-b-c` and `?a_b_c` ends up in `_a_b_c`.

Symbol `_` and `?` are both converted to symbol `_`, a wildcard symbol.

## Utility Functions

    sort-clauses (rules)

    Destructively sort the rules by dictionary ordering.
    Useful for avoiding noncontiguous-clauses errors (rules for the same clauses are not written together).
    However, since it alters the ordering of the rules and Prolog checks the rules from top-to-bottom,
    it may corrupt the program especially when cut operator (!) is involved.
    
    Facts (rules without conditions) are given precedence to the standard rules.

    print-term-sexp ()
    
    This function returns a cl-prolog2 program for a prolog rule print-term-sexp,
    which prints a prolog term in a SEXP form,
    i.e. achieving a goal (print-term-sexp ?term) prints ?term in SEXP.

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.0 on X86-64 Linux 4.10.0-38-generic (author's environment)

Also, it depends on the following libraries:

+ trivia :
    
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Copyright 2017 Masataro Asai
Released under the MIT license
http://opensource.org/licenses/mit-license.php
