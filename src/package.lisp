#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog
  (:use :cl :trivia :alexandria :iterate))
(in-package :cl-prolog)

;; blah blah blah.

(named-readtables:in-readtable :fare-quasiquote)

;; ideas:
;; asdf integration (sexp-defined prolog)
;; batch execution
;; interactive execution



;; don't try to do things really complicated.
;; do not excessively try to support all subset of prolog.

(defun print-atom (stream atom colon at &rest rest)
  (declare (ignorable colon at rest))
  (ematch atom
    ((symbol :name (and name (string* #\?)))
     (write-string (string-capitalize name) stream :start 1))
    ((symbol name)
     (write-string (string-downcase name) stream))
    ((string)
     (format stream "'~a'" atom))
    (_
     (write atom :stream stream))))

(defun print-commas (stream list colon at &rest rest)
  (declare (ignorable colon at rest))
  (format stream "~{~/cl-prolog::print-atom/~^,~}" list))

(defun print-term (stream list colon at &rest rest)
  (declare (ignorable colon at rest))
  (format stream "~/cl-prolog::print-atom/(~/cl-prolog::print-commas/)" (car list) (cdr list)))

(defun print-fact (stream list colon at &rest rest)
  (declare (ignorable colon at rest))
  (format stream "~/cl-prolog::print-term/.~%" list))

(defun print-clause (stream list colon at &rest rest)
  (declare (ignorable colon at rest))
  (match list
    (`(,head ,@rest)
      (format stream "~/cl-prolog::print-term/ :- ~{~/cl-prolog::print-term/~^,~}.~%"
              head rest))))
  
  
;; > < is ->  + *
;; write-canonical
;; 
;; oh, ok, so, prolog DOES internally use the canonical form for infix
;; operators, so it is ok for us to provide them using prefix operators.
;; http://www.cse.unsw.edu.au/~billw/cs9414/notes/prolog/op.html
