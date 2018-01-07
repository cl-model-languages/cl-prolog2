#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.xsb
  (:use :cl :cl-prolog2 :cl-prolog2.impl)
  (:export
   #:xsb))
(in-package :cl-prolog2.xsb)

;; blah blah blah.
;; ./xsb --nobanner --quietload --noprompt

(defvar *xsb* (namestring (asdf:system-relative-pathname :cl-prolog2.xsb "XSB/bin/xsb")))

(defmethod run-prolog ((rules list) (prolog-designator (eql :xsb))
                       &key
                         (debug *debug-prolog*) args
                         (input *standard-input*)
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
         `(,*xsb* "--nobanner" "--quietload" "--noprompt"
                  "-e" ,(format nil "consult('~a')." input-file) ,@args) 
         :input input :output output :error error)))))



