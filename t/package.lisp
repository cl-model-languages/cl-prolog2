#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :cl-prolog.test
  (:use :cl
        :cl-prolog
        :fiveam
        :trivia :alexandria :iterate))
(in-package :cl-prolog.test)



(def-suite :cl-prolog)
(in-suite :cl-prolog)

;; run test with (run! test-name) 

(test cl-prolog

      (print-rule *standard-output* '(<-- (a b c) (a ?b c) (a b c)) nil nil)
      (print-rule *standard-output* '(<-- (a b c) (a _b c) (a b c)) nil nil)
      (print-rule *standard-output* '(a b c) nil nil)
      (print-rule *standard-output* '(member ?x (list* ?x _)) nil nil)
      (print-rule *standard-output* '(<-- (member ?x (list* _ ?r)) (member ?x ?r)) nil nil))



