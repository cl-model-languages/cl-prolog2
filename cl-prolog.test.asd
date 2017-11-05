#|
  This file is a part of cl-prolog project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage cl-prolog.test-asd
  (:use :cl :asdf))
(in-package :cl-prolog.test-asd)


(defsystem cl-prolog.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog"
  :license "LLGPL"
  :depends-on (:cl-prolog
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(5am:run! :cl-prolog)"))
))
