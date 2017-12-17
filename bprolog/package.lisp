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

(defmethod run-prolog ((rules list) (prolog-designator (eql :bprolog)) &key debug args)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.pl" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (dolist (r rules)
          (print-rule s r))
        (print-rule s '(:- (initialization halt))))
      ;; remove the banner
      (let* ((out (uiop:run-program `(,(namestring (asdf:system-relative-pathname :cl-prolog.bprolog "BProlog/bp"))
                                       "-i" ,input-file ,@args)
                                    :output :string))
             (pos (loop with count = 0
                     until (= count 2)
                     for i from 0
                     do
                       (when (char= (aref out i) #\Newline)
                         (incf count))
                     finally (return i))))
        (make-array (- (length out) pos 1)
                    :element-type 'character
                    :displaced-to out
                    :displaced-index-offset (1+ pos))))))

