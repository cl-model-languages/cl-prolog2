#|
  This file is a part of cl-prolog2 project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage cl-prolog2.test-asd
  (:use :cl :asdf))
(in-package :cl-prolog2.test-asd)


(defsystem cl-prolog2.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2"
  :license "MIT"
  :depends-on (:cl-prolog2
               :iterate
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(5am:run! :cl-prolog2)"))
))
