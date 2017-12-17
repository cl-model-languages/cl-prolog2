
(in-package :cl-prolog)

;;; API.

(defvar *debug-prolog* nil
  "Flag for debugging the input to the prolog interpreter.
 Enables verbose output when something is sent the interpreter.")

(defun print-rule (stream rule)
  (when *debug-prolog*
    (format t "~&; ~/cl-prolog::%print-rule/" rule))
  (%print-rule stream rule nil nil))

(defgeneric run-prolog (input prolog-designator &key debug args))

(defmacro with-temp ((var &key directory template (tmpdir "/tmp/") debug) &body body)
  "Create a temporary file, then remove the file by unwind-protect.
Most arguments are analogous to mktemp.
When DIRECTORY is non-nil, creates a directory instead.
When DEBUG is non-nil, it does not remove the directory so that you can investigate what happened inside the directory."
  (with-gensyms (command)
    `(let ((,var (uiop:run-program
                  (let ((,command (format nil "mktemp --tmpdir='~a' ~@[-d~*~] ~@[~a~]"
                                          ,tmpdir ,directory ,template)))
                    (if ,debug
                        (print ,command)
                        ,command))
                  :output '(:string :stripped t))))
       (unwind-protect
            (progn ,@body)
         (if ,debug
             (format t "~&not removing ~a for debugging" ,var)
             (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t))))))


