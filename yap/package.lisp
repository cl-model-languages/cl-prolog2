#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.yap
  (:use :cl :cl-prolog2 :cl-prolog2.impl)
  (:export
   #:yap))
(in-package :cl-prolog2.yap)

;; blah blah blah.

(defvar *yap* (namestring (asdf:system-relative-pathname :cl-prolog2.yap "yap/build/yap")))

(defmethod run-prolog ((rules list) (prolog-designator (eql :yap))
                       &key
                         (debug *debug-prolog*) args
                         (input "/dev/null")
                         (output :string)
                         (error *error-output*)
                         &allow-other-keys)
  (let ((*debug-prolog*
         (if (typep debug '(integer 0 3)) debug (if debug 2 0))))
    (with-temp (d :directory t :debug (<= 1 *debug-prolog*))
      (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug (<= 1 *debug-prolog*))
        (with-open-file (s input-file :direction :output :if-does-not-exist :error)
          (dolist (r rules)
            (print-rule s r)))

        (run-command-with-debug-print
         `(,*yap* "-l" ,input-file ,@args)
         :input input :output output :error error)))))

