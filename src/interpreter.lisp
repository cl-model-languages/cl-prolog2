
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

(defun send-rule (process rule)
  (send-rules process (list rule)))

#+(or)                                  ;doesnt seem to recognize EOT characters.
(defun send-rules (process rules)
  (with-prolog-io (process i o e)
    ;; enter the interactive mode
    (when *debug-prolog*
      (format t "~&; ~/cl-prolog::print-rule/" '(list user)))
    (print-rule i '(list user) nil nil)
    ;; enter rules
    (dolist (r rules)
      (when *debug-prolog*
        (format t "~&; ~/cl-prolog::print-rule/" r))
      (print-rule i r nil nil))
    (when *debug-prolog*
      (format t "~&; EOT"))
    (write-char #\EOT i)
    (finish-output i)
    ;; (assert (eq 'true. (read o)))
    ;; consume all outputs. Especially "true.".
    (clear-input o)))

(defun send-rules (process rules)
  (with-prolog-io (process i o e)
    ;; enter the interactive mode
    ;; (when *debug-prolog*
    ;;   (format t "~&; ~/cl-prolog::print-rule/" `(assertz (and ,@rules))))
    ;; (print-rule i  `(assertz (and ,@rules)) nil nil)
    (dolist (r rules)
      (when *debug-prolog*
        (format t "~&; ~/cl-prolog::print-rule/" `(assertz ,r)))
      (print-rule i  `(assertz ,r) nil nil)
      (finish-output i)
      (read-char o)
      (clear-input o))))


(defun send-query (process query callback)
  (with-prolog-io (process i o e)
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
