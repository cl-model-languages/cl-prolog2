#|
  This file is a part of cl-prolog.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog.bprolog
  (:use :cl :cl-prolog)
  (:export
   #:bprolog))
(in-package :cl-prolog.bprolog)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :bprolog)) &key debug)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.pl" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (dolist (r rules)
          (print-rule s r))
        (print-rule s '(:- (initialization halt))))
      
      (uiop:run-program `(,(namestring (asdf:system-relative-pathname :cl-prolog.bprolog "BProlog/bp"))
                           "-i" ,input-file) :output '(:string :stripped t)))))

