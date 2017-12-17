#|
  This file is a part of cl-prolog.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog.yap
  (:use :cl :cl-prolog)
  (:export
   #:yap))
(in-package :cl-prolog.yap)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :yap)) &key debug args)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.pl" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (print-rule s '(:- (set_prolog_flag unknown error)))
          (dolist (r rules)
            (print-rule s r))))
      (uiop:run-program `("yap" "-l" ,input-file ,@args) :output '(:string :stripped t)))))

