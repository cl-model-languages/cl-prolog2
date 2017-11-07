;;;; Autogenerated ASD file for system "CL-PROLOG"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem cl-prolog
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "MIT"
 :depends-on (:trivia
              :alexandria
              :iterate
              :trivia.quasiquote
              :external-program
              :trivial-garbage)
 :pathname "src"
 :components ((:file "package")
              (:file "compiler-macro")
              (:file "printers")
              (:file "interpreter"))
 :description "Common Interface to the ISO prolog implementations from Common Lisp"
 :in-order-to ((test-op (test-op :cl-prolog.test))))
