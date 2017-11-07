
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
                                  :output :stream))
    (tg:finalize instance
                 (lambda ()
                   (external-program:signal-process process 15)))))

(defun terminate (instance)
  (with-slots (process) instance
    (external-program:signal-process process 15)))

(defun call-with-prolog-io (instance callback)
  (with-slots (process) instance
    (with-accessors ((input external-program:process-input-stream)
                     (output external-program:process-output-stream)) process
      (funcall callback input output))))

(defmacro with-prolog-io ((process input output) &body body)
  `(call-with-prolog-io ,process (lambda (,input ,output)
                                   (declare (ignorable ,input ,output))
                                   ,@body)))

(defvar *debug-prolog* t
  "Flag for debugging the input to the prolog interpreter.
 Enables verbose output when something is sent the interpreter.")

(defun send-rule (process rule)
  (send-rules process (list rule)))

(defun send-rules (process rules)
  (with-prolog-io (process i o)
    ;; enter the interactive mode
    (print-rule i '(list user) nil nil)
    ;; enter rules
    (dolist (r rules)
      (when *debug-prolog*
        (format t "~&; ~/cl-prolog::print-rule/" r))
      (print-rule i r nil nil))
    (write-char #\EOT i)
    (finish-output i)
    ;; (assert (eq 'true. (read o)))
    ;; consume all outputs. Especially "true.".
    (clear-input o)))

(defun send-query (process query callback)
  (with-prolog-io (process i o)
    (when *debug-prolog* 
      (format t "~&; ~/cl-prolog::print-rule/" query))
    (print-rule i query nil nil)
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
