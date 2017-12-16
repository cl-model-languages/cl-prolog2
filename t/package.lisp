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

(test hello-world
  (with-prolog-process (p *interpreter-class*)
    (send-query p '(write "hello world\\n")
                (lambda (output)
                  (is (equal "hello world" (read-line output)))
                  nil))))

(test symbol-name-test
  (with-prolog-process (p *interpreter-class*)
    
    (finishes
      (send-rules p `((parent-of luke-skywalker anakin-skywalker))))
    
    (send-query p `(and (parent-of luke-skywalker ?x)
                        (write ?x)
                        (write "\\n")
                        fail)
                (lambda (output)
                  (iter (with start = (get-universal-time))
                        (for now = (get-universal-time))
                        (while (< (- now start) 3))
                        (until (listen output)))
                  (is (equal 'anakin-skywalker (print (read output))))
                  nil))
    
    (send-query p `(and (parent-of luke-skywalker ?who-is-it)
                        (write ?who-is-it)
                        (write "\\n")
                        fail)
                (lambda (output)
                  (iter (with start = (get-universal-time))
                        (for now = (get-universal-time))
                        (while (< (- now start) 3))
                        (until (listen output)))
                  (is (equal 'anakin-skywalker (read output)))
                  nil))))

(test factorial
  (with-prolog-process (p *interpreter-class*)
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
  (with-prolog-process (p *interpreter-class*)
    
    (finishes
      (send-rules p `(;; note: this overwrites swi-prolog's builtin
                      (member ?x (list* ?x ?rest))
                      (:- (member ?x (list* ?y ?rest))
                          (member ?x ?rest))
                      (:- (perm (list* ?x ?y) ?z)
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
