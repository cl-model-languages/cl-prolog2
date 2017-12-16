#|
  This file is a part of cl-prolog.swi project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog.swi
  (:use :cl :cl-prolog)
  (:export
   #:swi-prolog))
(in-package :cl-prolog.swi)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :swi)) &key debug)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.pl" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (dolist (r rules)
          (print-rule s r)))
      (string-trim '(#\Space #\Newline #\Return)
                   (uiop:run-program `("swipl" "--quiet" "-l" ,input-file) :output :string)))))
