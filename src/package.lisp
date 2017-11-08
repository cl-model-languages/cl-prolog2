#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

;;; package

(in-package :cl-user)
(defpackage cl-prolog
  (:use :cl :trivia :alexandria)
  (:export
   #:<--
   #:prolog-interpreter
   #:with-prolog-io
   #:send-rule
   #:send-rules
   #:send-query
   #:terminate
   #:print-rule))
(in-package :cl-prolog)

;;; comments

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
