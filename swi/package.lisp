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

(defmethod run-prolog ((rules list) (prolog-designator (eql :swi)) &key debug args (input *standard-input*) (output :string) (error *error-output*) &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      (let ((command `("swipl" "--quiet" "-l" ,input-file ,@args)))
        (when debug
          (format *error-output* "; ~{~a~^ ~}" command))
        (string-trim '(#\Space #\Newline #\Return)
                     (alexandria:unwind-protect-case ()
                         (uiop:run-program command :input input :output output :error error)
                       (:abort 
                        (format *error-output* "~&; command was: ~{~a~^ ~}" command)
                        (setf debug t))))))))
