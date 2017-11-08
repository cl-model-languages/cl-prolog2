
(in-package :cl-prolog)

(named-readtables:in-readtable :fare-quasiquote)

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

(define-compiler-macro %print-rule (&whole whole stream obj colon at &environment env)
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
