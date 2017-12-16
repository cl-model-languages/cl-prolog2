#|
  This file is a part of cl-prolog.yap project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prolog.yap
  (:use :cl :cl-prolog)
  (:export
   #:yap))
(in-package :cl-prolog.yap)

;; blah blah blah.

(defclass yap (prolog-interpreter)
  ()
  (:default-initargs :program "yap" :default-args '()))

#+(or)
(defmethod send-rules :after ((process yap) (rules list))
  (with-prolog-io (process i o e)
    (assert (string= 'yes (read o)))))

#+(or)
(defmethod send-query :after ((process prolog-interpreter) query callback)
  (with-prolog-io (process i o e)
    ))

(defmethod initialize-instance :after ((instance yap) &key &allow-other-keys)
  (send-rule instance '(:- (set_prolog_flag unknown error))))

(pushnew '(:yap . yap) *interpreter-classes* :test 'equal)
