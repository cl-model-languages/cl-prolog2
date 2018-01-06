#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.bprolog
  (:use :cl :cl-prolog2)
  (:export
   #:bprolog))
(in-package :cl-prolog2.bprolog)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :bprolog)) &key debug args (input *standard-input*) (output :string) (error *error-output*) &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      ;; remove the banner
      (when debug
        (format *error-output* "; ~{~a~^ ~}" `(,(namestring (asdf:system-relative-pathname :cl-prolog2.bprolog "BProlog/bp"))
                                   "-i" ,input-file ,@args)))
      (let* ((out (alexandria:unwind-protect-case ()
                      (uiop:run-program `(,(namestring (asdf:system-relative-pathname :cl-prolog2.bprolog "BProlog/bp"))
                                           "-i" ,input-file ,@args)
                                        :input input
                                        :output output
                                        :error error)
                    (:abort (setf debug t))))
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

