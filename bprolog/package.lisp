#|
  This file is a part of cl-prolog2.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog2.bprolog
  (:use :cl :cl-prolog2 :cl-prolog2.impl :trivia)
  (:export
   #:bprolog))
(in-package :cl-prolog2.bprolog)

;; blah blah blah.

(defmethod run-prolog ((rules list) (prolog-designator (eql :bprolog))
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

        (let ((out (run-command-with-debug-print
                    `(,(namestring (asdf:system-relative-pathname :cl-prolog2.bprolog "BProlog/bp"))
                       "-i" ,input-file ,@args)
                    :input input :output output :error error)))
          (with-input-from-string (sin out)
            ;; skip the banner
            (read-line sin)
            (read-line sin)
            (with-output-to-string (sout)
              (do () ((not (listen sin)))
                (let ((line (read-line sin)))
                  (match line
                    ((string* #\* #\* #\Space #\E #\r #\r #\o #\r)
                     ;; skip next line too
                     (if (= 3 *debug-prolog*)
                         (progn
                           (warn "Skipping line: ~s" line)
                           (warn "Skipping line: ~s" (read-line sin)))
                         (read-line sin)))
                    (_
                     (write-string line sout))))))))))))

