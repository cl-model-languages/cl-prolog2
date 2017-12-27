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

(defmethod run-prolog ((rules list) (prolog-designator (eql :bprolog)) &key debug args &allow-other-keys)
  (with-temp (d :directory t :debug debug)
    (with-temp (input-file :tmpdir d :template "XXXXXX.prolog" :debug debug)
      (with-open-file (s input-file :direction :output :if-does-not-exist :error)
        (let ((*debug-prolog* debug))
          (dolist (r rules)
            (print-rule s r))))
      ;; remove the banner
      (when debug
        (format t "; ~{~a~^ ~}" `(,(namestring (asdf:system-relative-pathname :cl-prolog.bprolog "BProlog/bp"))
                                   "-i" ,input-file ,@args)))
      (let* ((out (alexandria:unwind-protect-case ()
                      (uiop:run-program `(,(namestring (asdf:system-relative-pathname :cl-prolog.bprolog "BProlog/bp"))
                                           "-i" ,input-file ,@args)
                                        :output :string)
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

