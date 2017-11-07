#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

;;; package

(in-package :cl-user)
(defpackage cl-prolog
  (:use :cl :trivia :alexandria :iterate)
  (:export
   #:<--
   #:prolog-interpreter
   #:with-prolog-io
   #:send-rule
   #:send-rules
   #:send-query))
(in-package :cl-prolog)

;;; printers

(named-readtables:in-readtable :fare-quasiquote)

;; ideas:
;; asdf integration (sexp-defined prolog)
;; batch execution
;; interactive execution

;; don't try to do things really complicated.
;; do not excessively try to support all subset of prolog.

(declaim (ftype (function (stream * boolean boolean))
                print-commas
                print-term
                print-rule))

(defun print-commas (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~{~/cl-prolog::print-term/~^,~}" list))

(defun print-semicolons (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~{~/cl-prolog::print-term/~^;~}" list))

(setf trivia:*arity-check-by-test-call* nil)

(defun print-term (stream term colon at)
  (declare (ignorable colon at))
  (ematch term
    ((symbol :name (and name (string* #\?)))
     (write-string (string-capitalize name) stream :start 1)
     ;; (write-char #\_ stream)
     ;; (write-string name stream :start 1)
     )
    ((symbol name)
     (write-string (string-downcase name) stream))
    ((string*)
     (format stream "'~a'" term))
    ((number)
     (write term :stream stream))
    (`(list ,@elements)
      (format stream "[~/cl-prolog::print-commas/]" elements))
    (`(list* ,@elements)
      (format stream "[~/cl-prolog::print-commas/|~/cl-prolog::print-term/]" (butlast elements) (lastcar elements)))
    (`(not ,term)
      (format stream "\\+ ~/cl-prolog::print-term/" term))
    (`(and ,@terms)
      (format stream "(~/cl-prolog::print-commas/)" terms))
    (`(or ,@terms)
      (format stream "(~/cl-prolog::print-semicolons/)" terms))
    (`(:- ,@arguments)
      (format stream ":-(~/cl-prolog::print-commas/)" arguments))
    (`(,functor ,@arguments)
      (format stream "~/cl-prolog::print-term/(~/cl-prolog::print-commas/)" functor arguments))))

(defun print-rule (stream list colon at)
  (declare (ignorable colon at))
  (match list
    (`(,(or :- '<--) ,head ,@rest)
      (format stream "~/cl-prolog::print-term/ :- ~{~/cl-prolog::print-term/~^,~}.~%"
              head rest))
    (_
     (format stream "~/cl-prolog::print-term/.~%" list))))

(defun constant-fold-printer (env whole)
  (match whole
    ((or `(,fn ,stream ,obj ,_ ,_)
         `(funcall ,fn ,stream ,obj ,_ ,_))
      (if (constantp obj env)
          `(write-string ,(with-output-to-string (s)
                            (funcall fn s obj nil nil))
                         ,stream)
          whole))
    (_
     whole)))

(define-compiler-macro print-rule (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-term (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-commas (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))
(define-compiler-macro print-semicolons (&whole whole stream obj colon at &environment env)
  (declare (ignorable stream obj colon at))
  (constant-fold-printer env whole))

;;; comments

;; (defun aaa (list)
;;   list)
;; (define-compiler-macro aaa (&whole whole list &environment env)
;;   (if (constantp list env)
;;       (progn (print :constant!)
;;              whole)
;;       (progn (print :not-constant!)
;;              whole)))
;; (defun fn (list)
;;   (aaa list)
;;   (aaa '(a b c)))

;; > < is ->  + *
;; write-canonical
;; 
;; oh, ok, so, prolog DOES internally use the canonical form for infix
;; operators, so it is ok for us to provide them using prefix operators.
;; http://www.cse.unsw.edu.au/~billw/cs9414/notes/prolog/op.html

;; there are more special cases: list notation [] and [|], which is already handled.

;; Retrieving the answer from prolog: findone or findall.


;;; API.

(defclass prolog-interpreter ()
  (process
   (program :initarg :program :allocation :class)
   (default-args :initarg :default-args :allocation :class)))

(defmethod initialize-instance ((instance prolog-interpreter) &key args &allow-other-keys)
  (with-slots (process program default-args) instance
    (setf process
          (external-program:start program (or args default-args) :input :stream :output :stream))
    (tg:finalize instance
                 (lambda ()
                   (external-program:signal-process process 15)))))

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
        (format t "~/cl-prolog::print-rule/" r))
      (print-rule i r nil nil))
    (write-char #\EOT i)
    (finish-output i)
    (assert (eq 'true. (read o)))))

(defun send-query (process query callback)
  (with-prolog-io (process i o)
    (when *debug-prolog* 
      (format t "~/cl-prolog::print-rule/" query))
    (print-rule i query nil nil)
    (finish-output i)
    (unwind-protect-case ()
        (loop
           (funcall callback o)
           (write-char #\; i))
      (:abort (write-char #\. i)))))
