
# Cl-Prolog2 - Common Interface to the ISO Prolog implementations from Common Lisp

[![Build Status](https://travis-ci.org/guicho271828/cl-prolog2.svg?branch=master)](https://travis-ci.org/guicho271828/cl-prolog2)

This is a realization of Marc Kuo's ["modelling approach to OR (operations research)"](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/)
for Prolog language.

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke State-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.
Choose the one with the *best performance for your application*.

**News** We changed the implementation to use the batch-mode only, since the behavior of Prolog top-level loop
is not defined in the ISO standard, and it is hard to maintain the compatibility between different interpreters.

**News** Added support for XSB and GNU Prolog.

**News** Added a CLI interface (roswell script) which can process a sexp-based prolog file directly from the command line.

**News** `*debug-level*` can control the verbosity of the output.

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

* [SWI-prolog](http://swi-prolog.org/), the most actively developed prolog system.
* [YAP](https://www.dcc.fc.up.pt/~vsc/Yap/), another well-maintained prolog system.
* [XSB](http://xsb.sourceforge.net/), famous for introducing tabling semantics.
* [B-Prolog](http://www.picat-lang.org/bprolog),
  originally a commercial implementation by Afany Software, currently in the public domain (personal communication).
  The [benchmark](http://www.picat-lang.org/bprolog/performance.htm) was taken many years ago, but
  our internal testing reconfirmed this performance.
  
* [GNU Prolog](http://www.gprolog.org/) is a prolog compiler that compiles
  Prolog to a native assembly and generates a standalone executable.

Recently, many prolog systems contain an interface to constraint/linear programming solvers.
Also, with tabling semantics (basically a form of automated memoization), certain programs
runs faster and can be conveniently written without much consideration on the termination.
I believe there is a good motivation to reevaluate Prolog as a good "middleware language" for expressing and solving complex problems.

Tabling (also known as SLG resolution or Well Founded Semantics) is supported by
all implementations above, but with various range of feature support. See the later section for difference.

## API

The ASDF system `cl-prolog2` does not provide implementations, but merely the API to those implementations.
The sub-libraries of `cl-prolog2` are in the corresponding sub-directories.

They should implement a method `(run-prolog rules prolog-designator &key debug args &allow-other-keys)`,
where `prolog-designator` is a keyword symbol such as `:swi` or `:yap`.
The function returns the output of the process as a string.

* `:debug` --- Non-nil means debug level 3. If a debug level is given, it is used. By default it uses special variable `*debug-prolog*`.
* `:args` --- provides additional command line argument to the prolog
  interpreter. For example, `:args '("-g" "main")` for swi-prolog says that the top-level goal is `main`.

**We don't provide a parser for Prolog output** and, therefore,
**formatting the output should be done by Prolog** or you should **write a parser from lisp**.
Also, consider using `print-term-sexp` below. 

Debug level of prolog is controlled by `*debug-prolog*`.

```
*DEBUG-PROLOG* names a special variable:
  Declared type: (UNSIGNED-BYTE 2)
  Value: 0
  Documentation:
    Flag (0-3) for debugging the input to the prolog interpreter.
     0  : disabled.
     >=1: print the command line
     >=2: print the prolog output
     3  : most verbose. print misc messages. 
```

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

## CLI interface (roswell script)

To use the CLI interface, install [roswell](https://roswell.github.io/) and run `ros install cl-prolog2`.
(*note*: until it is included in Quicklisp, use `ros install guicho271828/cl-prolog2` instead.)
A usage example is in `examples/` directory.

    Usage:
    cl-prolog2 [-E] [-d] [-s] [-b BACKEND] FILE ARGS...
    
    This program takes a file written in cl-prolog2 sexp format and process it.
    Normally it converts the input to a regular prolog file and invokes the specified prolog interpreter/compiler.
    
    Options:
    
    -E : Preprocessing-only mode. It writes the regular prolog file to the standard output.
    -d : debug mode, the preprocessed prolog file is echoed in the error output.
    -s : it appends the print-sexp code to the prolog output.
    -b BACKEND : change the backend, e.g. swi, yap, xsb, gprolog, bprolog. [default: swi]
    ARGS... : ARGS is a set of additional parameters given to each prolog processor.

## Utility Functions

    cl-prolog2:sort-clauses (rules)

    Destructively sort the rules by dictionary ordering.
    Useful for avoiding noncontiguous-clauses errors (rules for the same clauses are not written together).
    However, since it alters the ordering of the rules and Prolog checks the rules from top-to-bottom,
    it may corrupt the program especially when cut operator (!) is involved.
    
    Facts (rules without conditions) are given precedence to the standard rules.

    cl-prolog2:print-term-sexp ()
    
    This function returns a cl-prolog2 program for a prolog rule print-term-sexp,
    which prints a prolog term in a SEXP form,
    i.e. achieving a goal (print-term-sexp ?term) prints ?term in SEXP.

## To what extent prolog systems are compatible to each other?

No implementations are complete. Each system has its own problem. Below we list some minor/major incompatibility and advantage/disadvantage that I encountered:

* SWI:
  * The version which comes from the package manager is usually not compiled with tabling support.
    On ubuntu/debian, cl-prolog2 pulls from [the official PPA repository](https://launchpad.net/~swi-prolog) which support it.
  * Tabling works, but [the speed is not optimized compared to other systems](http://www.swi-prolog.org/pldoc/man?section=tabling#sec:A.35.1.2).
  * Tabling seems to have a problem when combined with `findall/3`, spewing "No permission to append findall-bag `0' (continuation in findall/3 generator?)".
  * Mode-directed tabling compatible to YAP, XSB, B-Prolog is supported. The support is rather a superset of these.
* YAP:
  * The version which comes from the package manager is usually not compiled with tabling support.
  * We provide a `Makefile` in `yap/` subdirectory. This `Makefile` clones from the official repository, compiles it with tabling support and make it recognized by cl-prolog2.
  * When built from the source, you can also enable a parallelism optimization.
  * Somewhat maintained.
  * Libraries are compatible with SWI-prolog.
* XSB:
  * Development seems to be frozen.
  * Spews warning at various places.
  * Many useful non-ISO library functions in SWI are missing. (min_list, max_list etc)
* B-Prolog:
  * Development seems to be frozen.
  * It writes the error/warning message to the standard output, rather than the standard error.
    This is very inconvenient, so we wrote a custom filter that removes these errors.
  * It does not support more than 32 tabled predicates in a single clause. It reports that it is an error,
    but it seems like it just disables the tabling.
  * `(:- initialization main)` does not prevent printing the banner. CL-PROLOG2 skips reading this banner and returns a substring, but this method may be fragile.
  * Many useful non-ISO library functions in SWI are missing. (min_list, max_list etc)
* GNU Prolog:
  * The `gprolog` interpreter that comes with GNU-prolog is almost broken, as far as I tested it.
    Thus we disabled the interpreter mode, and use native-compiler `gplc` only.
  * `gprolog` does not seem to recognize the predicates defined in the same file.
  * `gprolog` spews lots of messages when consulting a file.
  * `gprolog` does not suppress banner with `(:- initialization main)`.
  * Many useful non-ISO library functions in SWI are missing. (min_list, max_list etc)

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.0 on X86-64 Linux 4.10.0-38-generic (author's environment)

To test the library, you need swi-prolog, gprolog, xsb, bprolog or yap prolog.
Specific notes for implementations:

+ For swi-prolog on ubuntu/debian, we recommend installing the latest from ppa:swi-prolog/stable .
+ For xsb, yap and bprolog, run the Makefile in each subdirectory (e.g. [[./bprolog/]] ) to download the binaries.
  Specifically, yap is compiled with tabling support which is missing in the Debian package.
+ gprolog is available from most package managers, but they are difficult to run.

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Copyright 2017 Masataro Asai
Released under the MIT license
http://opensource.org/licenses/mit-license.php
