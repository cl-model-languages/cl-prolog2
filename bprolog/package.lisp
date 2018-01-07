#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.bprolog
  (:use :cl :cl-prolog2 :trivia :iterate)
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
      (let ((command `(,(namestring (asdf:system-relative-pathname :cl-prolog2.bprolog "BProlog/bp"))
                        "-i" ,input-file ,@args)))
        (when debug
          (format *error-output* "; ~{~a~^ ~}" command))
        (let* ((out (alexandria:unwind-protect-case ()
                        (uiop:run-program command
                                          :input input
                                          :output output
                                          :error error)
                      (:abort (setf debug t)))))
          (with-input-from-string (sin out)
            ;; skip the banner
            (read-line sin)
            (read-line sin)
            (with-output-to-string (sout)
              (do () ((not (listen sin)))
                (let ((line (read-line sin)))
                  (match line
                    ((string* #\* #\* #\Space #\E #\r #\r #\o #\r)
                     (read-line sin) ; skip next line too
                     )
                    (_
                     (write-string line sout))))))))))))

