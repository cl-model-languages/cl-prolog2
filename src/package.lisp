#|
  This file is a part of cl-prolog2 project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

;;; package

(in-package :cl-user)

(defpackage cl-prolog2
  (:export
   #:run-prolog
   #:*debug-prolog*
   #:sort-clauses
   #:print-sexp))

(defpackage cl-prolog2.impl
  (:use :cl :trivia :alexandria :cl-prolog2)
  (:export
   #:print-rule
   #:with-temp
   #:run-command-with-debug-print))

