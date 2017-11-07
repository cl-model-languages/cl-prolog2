#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog
  (:use :cl :trivia :alexandria :iterate)
  (:export
   #:prolog-process
   #:send-rule
   #:<--
   #:print-rule))
(in-package :cl-prolog)

;; blah blah blah.

(named-readtables:in-readtable :fare-quasiquote)

;; ideas:
;; asdf integration (sexp-defined prolog)
;; batch execution
;; interactive execution



;; don't try to do things really complicated.
;; do not excessively try to support all subset of prolog.

(declaim (ftype (function (stream * boolean boolean))
                print-commas
                print-term
                print-rule))

(defun print-commas (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~{~/cl-prolog::print-term/~^,~}" list))

(defun print-semicolons (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~{~/cl-prolog::print-term/~^;~}" list))

(setf trivia:*arity-check-by-test-call* nil)

(defun print-term (stream term colon at)
  (declare (ignorable colon at))
  (ematch term
    ((symbol :name (and name (string* #\?)))
     (write-string (string-capitalize name) stream :start 1)
     ;; (write-char #\_ stream)
     ;; (write-string name stream :start 1)
     )
    ((symbol name)
     (write-string (string-downcase name) stream))
    ((string*)
     (format stream "'~a'" term))
    ((number)
     (write term :stream stream))
    (`(list ,@elements)
      (format stream "[~/cl-prolog::print-commas/]" elements))
    (`(list* ,@elements)
      (format stream "[~/cl-prolog::print-commas/|~/cl-prolog::print-term/]" (butlast elements) (lastcar elements)))
    (`(not ,term)
      (format stream "\\+ ~/cl-prolog::print-term/" term))
    (`(and ,@terms)
      (format stream "(~/cl-prolog::print-commas/)" terms))
    (`(or ,@terms)
      (format stream "(~/cl-prolog::print-semicolons/)" terms))
    (`(:- ,@arguments)
      (format stream ":-(~/cl-prolog::print-commas/)" arguments))
    (`(,functor ,@arguments)
      (format stream "~/cl-prolog::print-term/(~/cl-prolog::print-commas/)" functor arguments))))


(defun print-rule (stream list colon at)
  (declare (ignorable colon at))
  (match list
    (`(,(or :- '<--) ,head ,@rest)
      (format stream "~/cl-prolog::print-term/ :- ~{~/cl-prolog::print-term/~^,~}.~%"
              head rest))
    (_
     (format stream "~/cl-prolog::print-term/.~%" list))))

(defun constant-fold-printer (env whole)
  (match whole
    ((or `(,fn ,stream ,obj ,_ ,_)
         `(funcall ,fn ,stream ,obj ,_ ,_))
      (if (constantp obj env)
          `(write-string ,(with-output-to-string (s)
                            (funcall fn s obj nil nil))
                         ,stream)
          whole))
    (_
     whole)))

(define-compiler-macro print-rule (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-term (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-commas (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-semicolons (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))

;; (defun aaa (list)
;;   list)
;; (define-compiler-macro aaa (&whole whole list &environment env)
;;   (if (constantp list env)
;;       (progn (print :constant!)
;;              whole)
;;       (progn (print :not-constant!)
;;              whole)))
;; (defun fn (list)
;;   (aaa list)
;;   (aaa '(a b c)))

;; > < is ->  + *
;; write-canonical
;; 
;; oh, ok, so, prolog DOES internally use the canonical form for infix
;; operators, so it is ok for us to provide them using prefix operators.
;; http://www.cse.unsw.edu.au/~billw/cs9414/notes/prolog/op.html

;; there are more special cases: list notation [] and [|], which is already handled.

;; Retrieving the answer from prolog: findone or findall.


;; API.

(defclass prolog-process () ())

(defgeneric send-rule (process rule callback))

