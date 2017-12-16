#|
  This file is a part of cl-prolog.swi project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog.swi
  (:use :cl :cl-prolog)
  (:export
   #:swi-prolog))
(in-package :cl-prolog.swi)

;; blah blah blah.

(defclass swi (prolog-interpreter)
  ()
  (:default-initargs :program "swipl" :default-args '("--quiet")))

(defmethod send-rules :after ((process swi) (rules list))
  (with-prolog-io (process i o e)
    (assert (string= 'true. (read o)))))

#+(or)
(defmethod send-query :after ((process prolog-interpreter) query callback)
  (with-prolog-io (process i o e)
    ))

(pushnew '(:swi . swi) *interpreter-classes* :test 'equal)
