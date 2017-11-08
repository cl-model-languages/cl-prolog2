#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :cl-prolog.test
  (:use :cl
        :cl-prolog
        :fiveam
        :iterate
        :trivia :alexandria)
  (:shadow :terminate)
  (:export
   #:test-implementation))
(in-package :cl-prolog.test)



(def-suite :cl-prolog)
(in-suite :cl-prolog)

;; run test with (run! test-name) 

(test cl-prolog
  (print-rule *standard-output* '(<-- (a b c) (a ?b c) (a b c)) nil nil)
  (print-rule *standard-output* '(<-- (a b c) (a _b c) (a b c)) nil nil)
  (print-rule *standard-output* '(a b c) nil nil)
  (print-rule *standard-output* '(member ?x (list* ?x _)) nil nil)
  (print-rule *standard-output* '(<-- (member ?x (list* _ ?r)) (member ?x ?r)) nil nil))

(def-suite :cl-prolog.impl)
(in-suite :cl-prolog.impl)

(defvar *interpreter-class*)

(defun test-implementation (*interpreter-class*)
  (run! :cl-prolog.impl))

(defmacro with-impl ((var) &body body)
  `(let ((,var (make-instance *interpreter-class*)))
     (unwind-protect
          (progn ,@body)
       (cl-prolog:terminate ,var))))

(test hello-world
  (with-impl (p)
    (send-query p '(write "hello world\\n")
                (lambda (output)
                  (is (equal "hello world" (read-line output)))
                  nil))))

(test factorial
  (with-impl (p)
    (finishes
      (send-rules p `((factorial 0 1)
                      (:- (factorial ?n ?f)
                          (and (> ?n 0)
                               (is ?n1 (- ?n 1))
                               (factorial ?n1 ?f1)
                               (is ?f (* ?n ?f1)))))))
    
    (send-query p `(and (factorial 3 ?w)
                        (write ?w)
                        (write "\\n")
                        fail)
                (lambda (output)
                  (iter (with start = (get-universal-time))
                        (for now = (get-universal-time))
                        (when (> (- now start) 3)
                          (error "timeout!"))
                        (until (listen output)))
                  (is (eql #\6 (peek-char nil output)))
                  (is (= 6 (read output)))
                  nil))))

(test queens
  ;; https://www.metalevel.at/queens/
  (with-impl (p)
    
    (finishes
      (send-rules p `((:- (perm (list* ?x ?y) ?z)
                          (perm ?y ?w)
                          (takeout ?x ?z ?w))
                      (perm (list) (list))
                      (takeout ?x (list* ?x ?r) ?r)
                      (:- (takeout ?x (list* ?f ?r) (list* ?f ?s))
                          (takeout ?x ?r ?s))
                      (:- (solve ?p)
                          (perm (list 1 2 3 4 5 6 7 8) ?p)
                          (combine (list 1 2 3 4 5 6 7 8) ?p ?s ?d)
                          (alldiff ?s)
                          (alldiff ?d))
                      (:- (combine (list* ?x1 ?x) (list* ?y1 ?y) (list* ?s1 ?s) (list* ?d1 ?d))
                          (is ?s1 (+ ?x1 ?y1))
                          (is ?d1 (- ?x1 ?y1))
                          (combine ?x ?y ?s ?d))
                      (combine (list) (list) (list) (list))
                      (:- (alldiff (list* ?x ?y))
                          (not (member ?x ?y))
                          (alldiff ?y))
                      (alldiff (list ?x)))))
    
    (send-query p `(and (setof ?p (solve ?p) ?set)
                        (length ?set ?l)
                        (write ?l)
                        (write "\\n")
                        fail)
                (lambda (output)
                  (iter (with start = (get-universal-time))
                        (for now = (get-universal-time))
                        (while (< (- now start) 3))
                        (until (listen output)))
                  (is (equal 92 (read output)))
                  nil))))
