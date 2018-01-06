#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.xsb
  (:use :cl :cl-prolog2)
  (:export
   #:xsb))
(in-package :cl-prolog2.xsb)

;; blah blah blah.
;; ./xsb --nobanner --quietload --noprompt

(defvar *xsb* (namestring (asdf:system-relative-pathname :cl-prolog2.xsb "XSB/bin/xsb")))

(defmethod run-prolog ((rules list) (prolog-designator (eql :xsb)) &key debug args &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      (let ((command `(,*xsb* "--nobanner" "--quietload" "--noprompt"
                               "-e" ,(format nil "consult('~a')." input-file) ,@args)))
        (when debug
          (format *error-output* "; ~{\"~a\"~^ ~}" command))
        (alexandria:unwind-protect-case ()
            (uiop:run-program command :output '(:string :stripped t))
          (:abort (setf debug t)))))))

