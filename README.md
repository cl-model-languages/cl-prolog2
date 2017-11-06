
# Cl-Prolog - Common Interface to the ISO Prolog implementations from Common Lisp

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke Stat-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.

It looks like https://github.com/keithj/cl-prolog has similar things in mind, and
it has more careful considerations regarding which names are accepted by the Prolog language spec.

However, I don't do complicated things; if it works it's fine.
Be just careful which character you use for your symbols.

## API

This library does not provide implementations, but merely the API to those implementations.
The sub-libraries of cl-prolog should implement the subclass of `prolog-process` and the following generic functions.
Instantiating a `prolog-process` should launch a corresponding background process.

    (defclass prolog-process () ())
    (defgeneric send-rule (process rule callback))

`send-rule` should send a single rule described in an S-exp, and receives the result.
`callback` should be a function of single argument `stream`, which is connected to the process output.
This callback is called when the output reached an end-of-file, which happens when the Prolog process returns an answer.
You can parse the result from the stream. *We don't provide a parser for Prolog output.* (at least at the moment.)

To continue for obtaining more answers, you should return from the fucntion normally.
When no more answers are necessary, you should perform a local exit by `go`, `return-from` or `throw`.
Upon the local exit, `unwind-protect` emits a period `.` to the input stream and tells Prolog to stop the query immediately.


## Query format

Query format mostly follows the Allegro Prolog (which is a fork of PAIP Prolog).

    rule : (<-- top-term top-term*) | top-term
    
    top-term : ('not term) | ( atom term* )
    
    term : builtins | ( atom term* ) | atom | variable | number | string
    
    variable : symbols starting with ? or _

    atom : other symbols
    
    builtins : ('list term*) | ('list* term*) | ('not term) | ('or term*) | ('and term*)
    
    
Variables are given to the Prolog interpreter as capitalized symbols.

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

Licensed under the LLGPL License.


