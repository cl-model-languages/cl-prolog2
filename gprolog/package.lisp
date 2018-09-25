#|
  This file is a part of cl-prolog2.gprolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.gprolog
  (:use :cl :cl-prolog2 :cl-prolog2.impl)
  (:export
   #:gprolog-prolog))
(in-package :cl-prolog2.gprolog)

;; blah blah blah.

;; broken, don't use.
#+(or)
(defmethod run-prolog ((rules list) (prolog-designator (eql :gprolog-interpreted)) &key debug args (input "/dev/null") (output :string) (error *error-output*) &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      (let ((command `("gprolog" "--init-goal" ,(format nil "consult('~a')" input-file) ,@args)))
        (when debug
          (format *error-output* "; ~{~s~^ ~}" command))
        (let* ((out (alexandria:unwind-protect-case ()
                        (uiop:run-program command
                                          :input input
                                          :output output
                                          :error error)
                      (:abort 
                       (format *error-output* "~&; command was: ~{~s~^ ~}" command)
                       (setf debug t))))
               ;; skip lines for byte-code compilation
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
                      :displaced-index-offset (1+ pos)))))))

(defmethod run-prolog ((rules list) (prolog-designator (eql :gprolog))
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

        
      (let* ((executable (namestring (make-pathname :type "out" :defaults input-file))))

        (run-command-with-debug-print
         `("gplc" ,input-file "-o" ,executable)
         :input input :output output :error error)

        (run-command-with-debug-print
         `(,executable)
         :input input :output output :error error))))))
