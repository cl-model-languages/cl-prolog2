
(in-package :cl-prolog2.impl)

;;; API.

(declaim ((integer 0 3) *debug-prolog*))
(defvar *debug-prolog* 0
  "Flag (0-3) for debugging the input to the prolog interpreter.
 0  : disabled.
 >=1: print the command line
 >=2: print the prolog output
 3  : most verbose. print misc messages. ")

(defun print-rule (stream rule)
  (when (<= 2 *debug-prolog*)
    (format *error-output* "~&; ~/cl-prolog2.impl::%print-rule/" rule))
  (%print-rule stream rule nil nil))

(defgeneric run-prolog (rules prolog-designator &key debug args input output error &allow-other-keys))

(defmacro with-temp ((var &key directory template (tmpdir "/tmp/") debug) &body body)
  "Create a temporary file, then remove the file by unwind-protect.
Most arguments are analogous to mktemp.
When DIRECTORY is non-nil, creates a directory instead.
DEBUG is a form.
When evaluated to non-nil, it does not remove the directory so that you can investigate what happened inside the directory.
It may be evaluated multiple times."
  (with-gensyms (command)
    `(let ((,var (uiop:run-program
                  (let ((,command (format nil "mktemp --tmpdir='~a' ~@[-d~*~] ~@[~a~]"
                                          ,tmpdir ,directory ,template)))
                    (when ,debug
                      (format *error-output* "~&; ~a~%" ,command))
                    ,command)
                  :output '(:string :stripped t))))
       (unwind-protect
            (progn ,@body)
         (if ,debug
             (format *error-output* "~&; not removing ~a for debugging~%" ,var)
             (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t))))))


(defun run-command-with-debug-print (command &rest args)
  (when (<= 1 *debug-prolog*)
    (format *error-output* "~&; ~{~a~^ ~}~%" command))
  (alexandria:unwind-protect-case ()
      (apply #'uiop:run-program command args)
    (:abort
     (format *error-output* "~&; command was: ~{~a~^ ~}~%" command)
     (format *error-output* "~&; setting the debug level to 3")
     (setf *debug-prolog* 3))))

