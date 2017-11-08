;;; printers

(in-package :cl-prolog)

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
                %print-rule))

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
    (`(:- ,head ,@rest)
      (format stream "(~/cl-prolog::print-term/ :- (~{~/cl-prolog::print-term/~^,~}))"
              head rest))
    (`(,functor ,@arguments)
      (format stream "~/cl-prolog::print-term/(~/cl-prolog::print-commas/)" functor arguments))))

(defun %print-rule (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~/cl-prolog::print-term/.~%" list))
