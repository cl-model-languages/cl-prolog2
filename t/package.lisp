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
   #:test-implementation
   #:*interpreter-class*))
(in-package :cl-prolog.test)



(def-suite :cl-prolog)
(in-suite :cl-prolog)

;; run test with (run! test-name) 

(test cl-prolog
  (finishes (print-rule *standard-output* 'atom))
  (finishes (print-rule *standard-output* 'atom-with-hyphen))
  (finishes (print-rule *standard-output* '(:- (a b c) (a ?b c) (a b c))))
  (finishes (print-rule *standard-output* '(:-(a b c) (a _b c) (a b c))))
  (finishes (print-rule *standard-output* '(a b c)))
  (finishes (print-rule *standard-output* '(member ?x (list* ?x _))))
  (finishes (print-rule *standard-output* '(:- (member ?x (list* _ ?r)) (member ?x ?r)))))

(def-suite :cl-prolog.impl)
(in-suite :cl-prolog.impl)

(defvar *interpreter-class*)

(defvar *debug* t)

(test hello-world
  (is (equal "hello world"
             (run-prolog '((:- main (write "hello world"))
                           (:- (initialization main)))
                         *interpreter-class* :debug *debug*))))

(test symbol-name-test
  (is (equal "'anakin-skywalker'"
             (run-prolog `((parent-of luke-skywalker anakin-skywalker)
                           (:- main
                               (parent-of luke-skywalker ?x)
                               (write_canonical ?x)
                               fail)
                           (:- (initialization main)))
                         *interpreter-class* :debug *debug*)))
  (is (equal "'anakin-skywalker'"
             (run-prolog `((parent-of luke-skywalker anakin-skywalker)
                           (:- main
                               (parent-of luke-skywalker ?who-is-it)
                               (write_canonical ?who-is-it)
                               fail)
                           (:- (initialization main)))
                         *interpreter-class* :debug *debug*))))

(test factorial
  (is (= 6
         (read-from-string
          (print
           (run-prolog `((factorial 0 1)
                         (:- (factorial ?n ?f)
                             (and (> ?n 0)
                                  (is ?n1 (- ?n 1))
                                  (factorial ?n1 ?f1)
                                  (is ?f (* ?n ?f1))))
                         (:- main
                             (factorial 3 ?w)
                             (write_canonical ?w))
                         (:- (initialization main)))
                       *interpreter-class* :debug *debug*))))))

(test queens
  ;; https://www.metalevel.at/queens/
  (is (= 92
         (read-from-string
          (print
           (run-prolog `(;; note: this overwrites swi-prolog's builtin
                         (member ?x (list* ?x ?rest))
                         (:- (member ?x (list* ?y ?rest))
                             (member ?x ?rest))
                         ;; perm
                         (:- (perm (list* ?x ?y) ?z)
                             (perm ?y ?w)
                             (takeout ?x ?z ?w))
                         (perm (list) (list))
                         ;; takeout
                         (takeout ?x (list* ?x ?r) ?r)
                         (:- (takeout ?x (list* ?f ?r) (list* ?f ?s))
                             (takeout ?x ?r ?s))
                         ;; solve
                         (:- (solve ?p)
                             (perm (list 1 2 3 4 5 6 7 8) ?p)
                             (combine (list 1 2 3 4 5 6 7 8) ?p ?s ?d)
                             (alldiff ?s)
                             (alldiff ?d))
                         ;; combine
                         (:- (combine (list* ?x1 ?x) (list* ?y1 ?y) (list* ?s1 ?s) (list* ?d1 ?d))
                             (is ?s1 (+ ?x1 ?y1))
                             (is ?d1 (- ?x1 ?y1))
                             (combine ?x ?y ?s ?d))
                         (combine (list) (list) (list) (list))
                         ;; alldiff
                         (:- (alldiff (list* ?x ?y))
                             (not (member ?x ?y))
                             (alldiff ?y))
                         (alldiff (list ?x))
                         ;; main
                         (:- main
                             (setof ?p (solve ?p) ?set)
                             (length ?set ?l)
                             (write_canonical ?l))
                         (:- (initialization main)))
                       *interpreter-class* :debug *debug*))))))
