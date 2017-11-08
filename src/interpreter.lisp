
(in-package :cl-prolog)

;;; API.

(defclass prolog-interpreter ()
  (process
   (program :initarg :program :allocation :class)
   (default-args :initarg :default-args :allocation :class)))

(defmethod initialize-instance :after ((instance prolog-interpreter) &key args &allow-other-keys)
  (with-slots (process program default-args) instance
    (setf process
          (external-program:start program (or args default-args)
                                  :input :stream
                                  :output :stream
                                  :error t))
    (tg:finalize instance
                 (lambda ()
                   (external-program:signal-process process 15)))))

(defun terminate (instance)
  (with-slots (process) instance
    (external-program:signal-process process 15)))

(defun call-with-prolog-io (instance callback)
  (with-slots (process) instance
    (with-accessors ((input external-program:process-input-stream)
                     (output external-program:process-output-stream)
                     (error external-program:process-error-stream)) process
      (funcall callback input output error))))

(defmacro with-prolog-io ((process input output error) &body body)
  `(call-with-prolog-io ,process (lambda (,input ,output ,error)
                                   (declare (ignorable ,input ,output ,error))
                                   ,@body)))

(defvar *debug-prolog* t
  "Flag for debugging the input to the prolog interpreter.
 Enables verbose output when something is sent the interpreter.")

(defun %print-rule (stream rule)
  (when *debug-prolog*
    (format t "~&; ~/cl-prolog::print-rule/" rule))
  (print-rule stream rule nil nil))

(defun send-rule (process rule)
  (send-rules process (list rule)))

#+(or)
(defun send-rules (process rules)
  ;; Doesnt seem to recognize EOT characters.
  ;; Maybe pty works, but :tty option for run-program is sbcl specific.
  ;; Can we avoid it from the shell script side?
  (with-prolog-io (process i o e)
    ;; enter the interactive mode
    (%print-rule i '(list user))
    ;; enter rules
    (dolist (r rules)
      (%print-rule i r))
    (when *debug-prolog*
      (format t "~&; EOT"))
    (write-char #\EOT i)
    (finish-output i)
    ;; (assert (eq 'true. (read o)))
    ;; consume all outputs. Especially "true.".
    (clear-input o)))

#+(or)
(defun send-rules (process rules)
  ;; Using assertz to enter the rule.
  (with-prolog-io (process i o e)
    ;; enter the interactive mode
    ;; (when *debug-prolog*
    ;;   (format t "~&; ~/cl-prolog::print-rule/" `(assertz (and ,@rules))))
    ;; (print-rule i  `(assertz (and ,@rules)) nil nil)
    (dolist (r rules)
      (%print-rule i `(assertz ,r))
      (finish-output i)
      (clear-input o))))

(defgeneric send-rules (process rules))
(defmethod send-rules ((process prolog-interpreter) (rules list))
  (with-prolog-io (process i o e)
    ;; enter the interactive mode
    (%print-rule i '(list user))
    ;; enter rules
    (dolist (r rules)
      (%print-rule i r))
    (%print-rule i 'end_of_file)
    (finish-output i)
    ;; (assert (eq 'true. (read o)))
    ;; consume all outputs. Especially "true.".
    (clear-input o)))



(defgeneric send-query (process query callback))
(defmethod send-query ((process prolog-interpreter) query callback)
  (with-prolog-io (process i o e)
    (%print-rule i query)
    (finish-output i)
    (unwind-protect
         (loop
            while (funcall callback o)
            do
            ;; callback may not consume all outputs; make sure no more output is left
              (clear-input o)
            ;; ask for more queries
              (write-char #\; i)
              (finish-output i))
      ;; no more queries
      (write-char #\. i)
      (finish-output i)
      (clear-input o))))
