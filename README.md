
# Cl-Prolog - Common Interface to the ISO Prolog implementations from Common Lisp

This is a realization of Marc Kuo's ["modelling approach to OR (operations research)"](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/)
for Prolog language.

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke State-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.
Choose the one with the *best performance for your application*.

**News** We changed the implementation to use the batch-mode only, since the behavior of Prolog top-level loop
is not defined in the ISO standard, and it is hard to maintain the compatibility between different interpreters.

## Related work

It looks like https://github.com/keithj/cl-prolog has similar things in mind, and
it has more careful considerations regarding which names are accepted by the Prolog language spec.
However, I don't do such a complicated thing; if it works it's fine.
Be just careful whichever character you use for your symbols.
Also, it seems to have painfully gone through the FFI binding. I definitely won't do the same.

There are already many lisp-to-prolog libraries, including the one mentioned above, but
also Allegro Prolog, PAIP prolog and the Prolog in On Lisp are the famous ones.
They rather implemented a Prolog system by itself, i.e., [programming OR approach](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/).

## API

The ASDF system `cl-prolog` does not provide implementations, but merely the API to those implementations.
The sub-libraries of `cl-prolog` are in the corresponding sub-directories.

They should implement a method `(run-prolog rules prolog-designator)`, where `prolog-designator` is a keyword symbol
such as `:swi` or `:yap`.
The function returns the output of the process as a string.

**We don't provide a parser for Prolog output** and, therefore,
**formatting the output should be done by Prolog** or you should **write a parser from lisp**.

## Query format

    rule : (:- top-term top-term*) | top-term
    
    top-term : ('not term) | ( atom term* )
    
    term : builtins | ( atom term* ) | atom | variable | number | string
    
    variable : symbols starting with ? or _

    atom : other symbols
    
    builtins : ('list term*) | ('list* term*) | ('not term) | ('or term*) | ('and term*)
    
Variables are given to the Prolog interpreter as underscored symbols, and
note that **all non-alphanumeric characters are converted to underscores**.

## Utility

    sort-clauses (rules)

    Destructively sort the rules by dictionary ordering.
    Useful for avoiding noncontiguous-clauses errors (rules for the same clauses are not written together).
    Facts (rules without conditions) are given precedence to the standard rules.

    print-term-sexp ()
    
    This function returns a cl-prolog program for a prolog rule print-term-sexp,
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
