
# Cl-Prolog - Common Interface to the ISO Prolog implementations from Common Lisp

This is a realization of Marc Kuo's ["modelling approach to OR (operations research)"](https://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/)
for Prolog language.

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke State-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.

* https://github.com/guicho271828/cl-prolog.swi
* https://github.com/guicho271828/cl-prolog.yap
* https://github.com/guicho271828/cl-prolog.xsb
* https://github.com/guicho271828/cl-prolog.gprolog

Choose the one with the best performance for your application.

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

This library does not provide implementations, but merely the API to those implementations.
The sub-libraries of cl-prolog should implement the subclass of `prolog-interpreter`
with `(:default-initargs :program "<program name>" :default-args '("--default" "shell" "arguments"))`.

    (defclass swi-prolog (prolog-interpreter) () (:default-initargs "swipl" :default-args '("--quiet")))

Having these default-initargs being set,
instantiating a `prolog-interpreter` launches a corresponding background process and holds a process object inside it.
You can override the arguments used to launch the process.

    (defvar *swi* (make-instance 'swi-prolog :args '("--nodebug" "--quiet")))

With an instance, you can send rules and queries to this background process.

    (send-rule process rule)
    (send-rules process rules)
    (send-query process query (lambda (stream) ...))

`send-rule/s` send a single/multiple SEXP rules as a valid Prolog program after a `[user].` command
(which allows the interpreter to add new rules/facts from the input),
then sends an EOT character to finish the input.

`send-query` is almost the same, but
`callback` should be a function of a single argument `stream`, which is
connected to the process output. You can parse the result from the stream while `(listen stream)` is true.
*We don't provide a parser for Prolog output* and *you must format the output from the Prolog side*.

To continue obtaining more answers, you should return from the function normally, in which case `;<Return>` is entered.
When no more answers are necessary, you should perform a local exit by `go`, `return-from` or `throw`.
Upon the local exit, `unwind-protect` emits a period `.<Return>` to the input stream and
tells Prolog to stop the query.

## Query format

Query format mostly follows the Allegro Prolog (which is a fork of PAIP Prolog).

    rule : (<-- top-term top-term*) | (:- top-term top-term*) | top-term
    
    top-term : ('not term) | ( atom term* )
    
    term : builtins | ( atom term* ) | atom | variable | number | string
    
    variable : symbols starting with ? or _

    atom : other symbols
    
    builtins : ('list term*) | ('list* term*) | ('not term) | ('or term*) | ('and term*)
    
Variables are given to the Prolog interpreter as underscored symbols. `:-` and `<--` are equivalent.

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
