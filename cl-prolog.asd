
(in-package :cl-user)
(defpackage cl-prolog-asd
  (:use :cl :asdf))
(in-package :cl-prolog-asd)


(defsystem cl-prolog
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "MIT"
  :depends-on (:trivia :alexandria :iterate :trivia.quasiquote)
  :pathname "src"
  :components ((:file "package"))
  :description "Common Interface to the ISO prolog implementations from Common Lisp"
  :in-order-to ((test-op (test-op :cl-prolog.test))))
