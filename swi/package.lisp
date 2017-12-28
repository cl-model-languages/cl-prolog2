#|
  This file is a part of cl-prolog2.swi project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.swi
  (:use :cl :cl-prolog2)
  (:export
   #:swi-prolog))
(in-package :cl-prolog2.swi)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :swi)) &key debug args &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      (when debug
        (format t "; ~{~a~^ ~}" `("swipl" "--quiet" "-l" ,input-file ,@args)))
      (string-trim '(#\Space #\Newline #\Return)
                   (alexandria:unwind-protect-case ()
                       (uiop:run-program `("swipl" "--quiet" "-l" ,input-file ,@args) :output :string)
                     (:abort (setf debug t)))))))
