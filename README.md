
# Cl-Prolog - Common Interface to the ISO Prolog implementations from Common Lisp

This library provides a transpiler from S-expression to
[ISO-standardized](https://www.iso.org/standard/21413.html) Prolog language,
and the sub-libraries that invoke Stat-of-the-Art Prolog compilers
such as SWI-prolog, XSB, Yap.

It looks like https://github.com/keithj/cl-prolog has similar things in mind, and
it has more careful considerations regarding which names are accepted by the Prolog language spec.

However, I don't do complicated things; if it works it's fine.
Be just careful which character you use for your symbols.

Variables are prefixed by `?`, such as `?X`, following the common conventions in lisp-based Prolog (e.g. On Lisp).
They are given to the Prolog interpreter as capitalized symbols.

## Usage



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


