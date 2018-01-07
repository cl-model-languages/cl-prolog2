;;; printers

(in-package :cl-prolog2.impl)

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
  (format stream "~{~/cl-prolog2::print-term/~^,~}" list))

(defun print-semicolons (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~{~/cl-prolog2::print-term/~^;~}" list))

(setf trivia:*arity-check-by-test-call* nil)

(defun print-term (stream term colon at)
  (declare (ignorable colon at))
  (ematch term
    ((symbol :name (and name (or (string* #\?)
                                 (string* #\_))))
     ;; variables
     (loop
        for c across name
        for i from 0
        do
          (if (zerop i)
              (write-char #\_ stream)
              (if (alphanumericp c)
                  (write-char (char-downcase c) stream)
                  (write-char #\_ stream)))))
    ((symbol name)
     (write-char #\' stream)
     ;; escape a backslash
     (loop
        for c across name
        do
          (if (char= #\\ c)
              (progn (write-char #\\ stream)
                     (write-char #\\ stream))
              (write-char (char-downcase c) stream)))
     (write-char #\' stream))
    ((string*)
     (format stream "'~a'" term))
    ((number)
     (write term :stream stream))
    (`(list ,@elements)
      (format stream "[~/cl-prolog2::print-commas/]" elements))
    (`(list* ,@elements)
      (format stream "[~/cl-prolog2::print-commas/|~/cl-prolog2::print-term/]" (butlast elements) (lastcar elements)))
    (`(not ,term)
      (format stream "\\+(~/cl-prolog2::print-term/)" term))
    (`(and ,@terms)
      (if terms
          (format stream "(~/cl-prolog2::print-commas/)" terms)
          (when (= 3 *debug-prolog*)
            (warn "empty conjunction is always a success."))))
    (`(or ,@terms)
      (if terms
          (format stream "(~/cl-prolog2::print-semicolons/)" terms)
          (progn
            (format stream "~/cl-prolog2::print-term/" 'fail)
            (when (= 3 *debug-prolog*)
              (warn "empty disjunction is always a success.")))))
    (`(:- ,head)
      (format stream "(:- ~/cl-prolog2::print-term/)"
              head))
    (`(:- ,head ,@rest)
      (format stream "(~/cl-prolog2::print-term/ :- (~{~/cl-prolog2::print-term/~^,~}))"
              head rest))
    (`(,functor ,@arguments)
      (format stream "~/cl-prolog2::print-term/(~/cl-prolog2::print-commas/)" functor arguments))))

(defun %print-rule (stream list colon at)
  (declare (ignorable colon at))
  (format stream "~/cl-prolog2::print-term/.~%" list))
